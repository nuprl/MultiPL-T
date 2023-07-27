use std::error::Error;
use std::fmt::Debug;
use std::sync::Arc;

use crate::repr::EvalResult;

use super::repr::Program;

use tokio::sync::Mutex;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::task::JoinSet;

#[derive(Debug)]
struct RunError(String);

impl<T> From<T> for RunError
where
    T: Debug + Error,
{
    fn from(value: T) -> Self {
        RunError(format!("{:?}", value))
    }
}

enum RunRes { 
    Succ,
    Fail
}
pub async fn spawn_runners(
    run_queue: Arc<Mutex<Receiver<Box<Program>>>>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
    attempt_limit: usize, 
    num_runners: usize,
) -> () {
    let mut tasks = JoinSet::new();
    for _ in 0..num_runners {
        let rq = run_queue.clone();
        let cq = compl_queue.clone();
        let fq = fin_queue.clone();
        tasks.spawn(run_programs(rq, cq, fq, attempt_limit));
    }
    while let Some(_) = tasks.join_next().await {
        ()
    }

}
async fn run_programs(
    run_queue: Arc<Mutex<Receiver<Box<Program>>>>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
    attempt_limit: usize,
) {
    loop {
        let mut prog: Box<Program> = {
            let mut chan = run_queue.clone().lock_owned().await;
            match chan.recv().await {
                None => return,
                Some(p) => p,
            }
        };
        match run_single_program(&prog).await {
            Ok(RunRes::Succ) => fin_queue.send(prog).await.unwrap(),
            Ok(RunRes::Fail) => { 
                if let Some(()) = prog.inc_attempts(attempt_limit) { 
                    compl_queue.send(prog).await.unwrap()
                }
            }
            Err(e) => { 
                let _ = compl_queue.send(prog).await.unwrap();
                eprint!("{:?}", e)
            }
        }
    }
}

async fn run_single_program(
    prog: &Program, 
) -> Result<RunRes, RunError> {
    let full_prog_text = format!("{}\n{}\n{}", prog.prompt, prog.completion, prog.tests);
    let args = [
        "simple_eval.py",
        "--prog-text",
        &full_prog_text,
        "--lang",
        &prog.language,
    ];
    let child = tokio::process::Command::new("python3")
        .args(args)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| RunError::from(e))?;
    let out = child
        .wait_with_output()
        .await
        .map_err(|e| RunError::from(e))?;
    let res = serde_json::from_str::<EvalResult>(
        std::str::from_utf8(&out.stdout).map_err(|e| RunError::from(e))?,
    )
    .map_err(|e| RunError::from(e))?;
    if res.status.to_lowercase().as_str() == "ok" { 
        Ok(RunRes::Succ)
    }
    else { 
        Ok(RunRes::Fail)
    }
}


