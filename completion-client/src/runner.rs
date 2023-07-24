use crate::repr::EvalResult;

use super::repr::Program;
use std::{
    process::Output,
    time::Duration,
};

use tokio::sync::mpsc::{Receiver, Sender};
use tokio::task::spawn;

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
// Since the MultiPL-E container runs with a timer, this could be superfluous
// however, the extra layer of indirection can't hurt.
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

async fn run_eval_container(
    mut prog: Box<Program>,
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
    .await;
    if let Some(res) = out { 
        match serde_json::from_str::<EvalResult>(std::str::from_utf8(&res.stdout).unwrap()) { 
            Ok(res) => if res.status.to_lowercase().as_str() == "ok"{ 
                let _ = fin_queue.send(prog).await;
            } else {
                if let Some(()) = (*prog).inc_attempts() {
                    let _ = compl_queue.send(prog).await;
                }
            }
            Err(_) => { 
                let _ = compl_queue.send(prog).await;
                eprintln!("Error decoding: {}", std::str::from_utf8(&res.stdout).expect("Extra bad"))
            }
        }
    }
    else {
        let _ = compl_queue.send(prog).await;
    }
}
