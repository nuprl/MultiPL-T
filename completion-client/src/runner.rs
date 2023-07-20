use super::repr::{Lang, Program};
use std::{
    process::Output,
    sync::atomic::{AtomicUsize, Ordering},
    time::Duration,
};

use lazy_static::lazy_static;
use tokio::{sync::mpsc::{Receiver, Sender}, io::AsyncWriteExt};
use tokio::task::spawn;
use tokio::fs::File;

lazy_static! {
    static ref FILE_IDX: AtomicUsize = AtomicUsize::new(0);
}
pub async fn prog_runner(
    mut run_queue: Receiver<Box<Program>>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
) {
    while let Some(prompt) = run_queue.recv().await {
        let cq = compl_queue.clone();
        let fq = fin_queue.clone();
        match prompt.lang {
            Lang::Python => spawn(async move { run_python(prompt, cq, fq).await }),
            Lang::Racket => spawn(async move { run_racket(prompt, cq, fq).await }),
            Lang::Typescript => spawn(async move { run_typescript(prompt, cq, fq).await }),
            Lang::OCaml => spawn(async move { run_ocaml(prompt, cq, fq).await }),
            Lang::Lua => spawn(async move { run_lua(prompt, cq, fq).await }),
        };
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

async fn create_temp_file(ext: &str) -> (File, String) {
    let idx = FILE_IDX.fetch_add(1, Ordering::SeqCst);
    // temp dir
    let temp_dir = std::env::temp_dir().join("codeexec");
    if !temp_dir.exists() {
        tokio::fs::create_dir_all(&temp_dir).await.unwrap();
    }
    let filename = format!("{}/{idx}.{ext}", temp_dir.to_string_lossy());
    let file = tokio::fs::File::create(&filename).await.expect("File creation failed");
    (file, filename)
}

async fn run_racket(
    prog: Box<Program>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
) -> () {
    let (mut file, file_path) = create_temp_file("rkt").await;
    let code = format!("{}", &prog.completion);
    let _ = file.write_all(code.as_bytes()).await.expect("Write should be successful");
    let output = run_program_with_timeout("racket", &[&file_path], Duration::from_secs(10)).await;
    dbg!("{:?}", &output);
    let succ = match output {
        None => false,
        Some(o) => o.status.code().unwrap() == 0 && o.stderr.len() == 0,
    };
    //spawn(async move { tokio::fs::remove_file(file_path).await });
    dispatch_result(succ, prog, compl_queue, fin_queue).await;
}

async fn run_lua(
    prog: Box<Program>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
) -> () {
    let (mut file, file_path) = create_temp_file("lua").await;
    let code = format!("{}\n{}\n{}", &prog.prompt, &prog.completion, &prog.tests);
    let _ = file.write_all(code.as_bytes()).await.expect("Write should be successful");
    let output = dbg!(run_program_with_timeout("lua", &[&file_path], Duration::from_secs(10)).await);
    let succ = match output.map(|o| o.status.code().unwrap_or(1)) {
        Some(0) => true,
        _ => false,
    };
    //spawn(async move { tokio::fs::remove_file(file_path).await });
    dispatch_result(succ, prog, compl_queue, fin_queue).await;
}

async fn run_ocaml(
    prog: Box<Program>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
) -> () {
    let (file, file_path) = create_temp_file("ml").await;
    let output = run_program_with_timeout("ocaml", &[&file_path], Duration::from_secs(10)).await;
    let succ = match output.map(|o| o.status.code().unwrap_or(1)) {
        Some(0) => true,
        _ => false,
    };
    spawn(async move { tokio::fs::remove_file(file_path).await });
    dispatch_result(succ, prog, compl_queue, fin_queue).await;
}
async fn run_python(
    _prog: Box<Program>,
    _compl_queue: Sender<Box<Program>>,
    _fin_queue: Sender<Box<Program>>,
) -> () {
    todo!()
}
async fn run_typescript(
    _prog: Box<Program>,
    _compl_queue: Sender<Box<Program>>,
    _fin_queue: Sender<Box<Program>>,
) -> () {
    todo!()
}

// Send a successful prompt to the fin_queue and an unsuccessful prompt
// back onto the compl_queue with another attempt
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
