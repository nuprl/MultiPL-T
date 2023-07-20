use crate::repr::EvalResult;

use super::repr::Program;
use std::{
    process::Output,
    sync::atomic::{AtomicUsize, Ordering},
    time::Duration,
};

use lazy_static::lazy_static;
use tokio::fs::File;
use tokio::task::spawn;
use tokio::{
    io::AsyncWriteExt,
    sync::mpsc::{Receiver, Sender},
};

lazy_static! {
    static ref FILE_IDX: AtomicUsize = AtomicUsize::new(0);
}
pub async fn prog_runner(
    mut run_queue: Receiver<Box<Program>>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
) {
    while let Some(prog) = run_queue.recv().await {
        let cq = compl_queue.clone();
        let fq = fin_queue.clone();
        spawn(async move { run_eval_container(prog, cq, fq).await });
    }
}

// Copied from Federico's runner
async fn run_program_with_timeout(
    program: &str,
    args: &[&str],
    timeout: Duration,
) -> Option<Output> {
    let child = tokio::process::Command::new(program)
        .args(args)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .ok()?;
    let child_id = child.id().unwrap();
    let output = tokio::time::timeout(timeout, child.wait_with_output()).await;
    match output {
        Ok(output) => match output {
            Ok(output) => Some(output),
            Err(_) => {
                let _ = tokio::process::Command::new("kill")
                    .arg("-9")
                    .arg(format!("{}", child_id))
                    .spawn();
                None
            }
        },
        Err(_) => {
            let _ = tokio::process::Command::new("kill")
                .arg("-9")
                .arg(format!("{}", child_id))
                .spawn();
            None
        }
    }
}

async fn create_temp_file(ext: &str) -> (File, String, String) {
    let idx = FILE_IDX.fetch_add(1, Ordering::SeqCst);
    // temp dir
    let temp_dir = std::env::temp_dir().join("codeexec");
    if !temp_dir.exists() {
        tokio::fs::create_dir_all(&temp_dir).await.unwrap();
    }
    let filename = format!("{idx}.{ext}");
    let file = tokio::fs::File::create(&filename)
        .await
        .expect("File creation failed");
    (file, temp_dir.to_string_lossy().to_string(), filename)
}

async fn run_eval_container(
    prog: Box<Program>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
) -> () {
    let full_prog_text = format!("{}\n{}\n{}", &prog.prompt, &prog.completion, &prog.tests);
    let temp_dir = std::env::temp_dir().join("codeexec");
    if !temp_dir.exists() {
        tokio::fs::create_dir_all(&temp_dir).await.unwrap();
    }
    let mount = format!("{}:/tmp", temp_dir.to_string_lossy().to_string());
    let out = run_program_with_timeout(
        "podman",
        &[
            "run",
            "--rm",
            "-v",
            &mount,
            "multipl-e-simple",
            "--prog-text",
            &full_prog_text,
            "--lang",
            &prog.language,
        ],
        Duration::from_secs(15),
    )
    .await
    .unwrap();
    let _ = dbg!(out.clone());
    let res = dbg!(std::str::from_utf8(&out.stdout).unwrap());
    let succ = match serde_json::from_str::<EvalResult>(res)
        .unwrap()
        .status
        .to_lowercase()
        .as_str()
    {
        "ok" => true,
        _ => false,
    };
    dispatch_result(succ, prog, compl_queue, fin_queue).await;
}

async fn dispatch_result(
    succ: bool,
    mut prog: Box<Program>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
) -> () {
    if succ {
        let _ = fin_queue.send(prog).await;
    } else {
        if let Some(()) = (*prog).inc_attempts() {
            let _ = compl_queue.send(prog).await;
        }
    }
}
