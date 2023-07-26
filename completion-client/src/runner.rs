use crate::repr::EvalResult;

use super::repr::Program;

use tokio::sync::mpsc::{Receiver, Sender, channel};
use tokio::task::spawn;

pub async fn prog_runner(
    mut run_queue: Receiver<Box<Program>>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
    attempt_limit: usize,
    conncurrent_programs: usize
) {
    let (tok_send, mut tok_recv) : (Sender<()>, Receiver<()>) = channel(conncurrent_programs + 1);
    for _ in 0..conncurrent_programs {
        let _ = tok_send.send(()).await;
    }
    while let (Some(prog), Some(())) = (run_queue.recv().await, tok_recv.recv().await) {
        let cq = compl_queue.clone();
        let fq = fin_queue.clone();
        let rts = tok_send.clone();
        spawn(async move { run_eval_container(prog, cq, fq, rts, attempt_limit).await });
    }
}

async fn run_eval_container(
    mut prog: Box<Program>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
    run_toks: Sender<()>,
    attempt_limit: usize,
) -> () {
    let full_prog_text = format!("{}\n{}\n{}", &prog.prompt, &prog.completion, &prog.tests);
    let temp_dir = std::env::temp_dir().join("codeexec");
    if !temp_dir.exists() {
        tokio::fs::create_dir_all(&temp_dir).await.unwrap();
    }
    let mount = format!("{}:/tmp", temp_dir.to_string_lossy().to_string());
    let args = [
        "run",
        "--rm",
        "-v",
        &mount,
        "multipl-e-simple",
        "--prog-text",
        &full_prog_text,
        "--lang",
        &prog.language,
    ];
    let child = tokio::process::Command::new("podman")
        .args(args)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("Child should spawn successfully");
    let out = child.wait_with_output().await;
    let _ = run_toks.send(()).await;
    if let Ok(res) = out {
        match serde_json::from_str::<EvalResult>(std::str::from_utf8(&res.stdout).unwrap()) {
            Ok(res) => {
                if res.status.to_lowercase().as_str() == "ok" {
                    let _ = fin_queue.send(prog).await;
                } else {
                    if let Some(()) = (*prog).inc_attempts(attempt_limit) {
                        let _ = compl_queue.send(prog).await;
                    }
                }
            }
            Err(_) => {
                eprintln!(
                    "Error decoding: {}, res: {:?}, prog:{}",
                    std::str::from_utf8(&res.stdout).expect("Extra bad"),
                    res,
                    &full_prog_text
                );
                let _ = compl_queue.send(prog).await;
            }
        }
    } else {
        let _ = compl_queue.send(prog).await;
        eprintln!("Child process failed: {:?}", out)
    }
}
