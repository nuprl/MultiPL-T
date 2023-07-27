use crate::repr::EvalResult;

use super::repr::Program;

use tokio::sync::mpsc::{channel, Receiver, Sender};
use tokio::task::spawn;

pub async fn prog_runner(
    mut run_queue: Receiver<Box<Program>>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
    attempt_limit: usize,
    conncurrent_programs: usize,
) {
    let (tok_send, mut tok_recv): (Sender<()>, Receiver<()>) = channel(conncurrent_programs + 1);
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
    let args = [
        "simple_eval.py",
        "--prog-text",
        &full_prog_text,
        "--lang",
        &prog.language,
    ];
    let child = match tokio::process::Command::new("python3")
        .args(args)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(e) => {
            let _ = compl_queue.send(prog).await;
            eprintln!(
                "Failed to run program: {}\n got error {:?}",
                &full_prog_text, e
            );
            return;
        }
    };
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
