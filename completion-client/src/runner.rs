use std::error::Error;
use std::fmt::Debug;

use crate::repr::EvalResult;

use super::mpmc::{recv_shared, SharedReceiver};
use super::repr::Program;

use tokio::io::AsyncWriteExt;
use tokio::sync::mpsc::Sender;
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
    Fail,
}
pub async fn spawn_runners(
    run_queue: SharedReceiver<Box<Program>>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
    log_queue: Sender<(String, Option<String>)>,
    read_toks: Sender<()>,
    attempt_limit: u32,
    num_runners: usize,
) -> () {
    let mut tasks = JoinSet::new();
    for _ in 0..num_runners {
        let rq = run_queue.clone();
        let cq = compl_queue.clone();
        let fq = fin_queue.clone();
        let lq = log_queue.clone();
        let rt = read_toks.clone();
        tasks.spawn(run_programs(rq, cq, fq, lq, rt, attempt_limit));
    }
    tasks.detach_all()
}
async fn run_programs(
    run_queue: SharedReceiver<Box<Program>>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
    log_queue: Sender<(String, Option<String>)>,
    read_toks: Sender<()>,
    attempt_limit: u32,
) {
    loop {
        let mut prog: Box<Program> = match recv_shared(run_queue.clone()).await {
            None => {
                println!("Early return from runners");
                return;
            }
            Some(p) => p,
        };
        match run_single_program(&prog).await {
            Ok(RunRes::Succ) => { 
                let _ = log_queue.send((format!("Success for {}:", &prog.name), None)).await;
                let _ = fin_queue.send(prog).await.unwrap();
                let _ = read_toks.send(()).await.unwrap();
            }
            Ok(RunRes::Fail) => {
                if let Some(()) = prog.inc_attempts(attempt_limit) {
                    let _ = compl_queue.send(prog).await.unwrap();
                } else {
                    let _ = log_queue.send((format!("No more attempts for {}", prog.name), None)).await;
                    let _ = read_toks.send(()).await;
                }
            }
            Err(e) => {
                let pstr = String::from(&*prog);
                if let Some(()) = prog.inc_attempts(attempt_limit) {
                    compl_queue.send(prog).await.unwrap();
                }
                else { 
                    let _ = log_queue.send((format!("No more attempts for {}", prog.name), None)).await;
                    let _ = read_toks.send(()).await;
                }
                let _ = log_queue.send((format!("{:?}", e), Some(pstr))).await;
            }
        }
    }
}

async fn run_single_program(prog: &Program) -> Result<RunRes, RunError> {
    let full_prog_text = String::from(prog);
    let args = [
        "simple_eval.py",
        "--lang",
        &prog.language,
    ];
    let mut child = tokio::process::Command::new("python3")
        .args(args)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| RunError(format!("Error running child: {:?}", e)))?;
    let mut child_stdin = child.stdin.take().expect("Child stdin");
    let _ = child_stdin.write_all(format!("{}", full_prog_text).as_bytes()).await;
    let _ = child_stdin.shutdown().await;
    drop(child_stdin);
    let out = child
        .wait_with_output()
        .await
        .map_err(|e| RunError::from(e))?;
    let res = serde_json::from_str::<EvalResult>(
        str_from_u8_nul_utf8(&out.stdout)
            .map_err(|e| RunError(format!("Error parsing utf8: {:?}", e)))?,
    )
    .map_err(|e| RunError(format!("Serialization Error: {:?}", e)))?;
    if res.status.to_lowercase().as_str() == "ok" {
        Ok(RunRes::Succ)
    } else {
        Ok(RunRes::Fail)
    }
}

//https://stackoverflow.com/a/42067321
fn str_from_u8_nul_utf8(utf8_src: &[u8]) -> Result<&str, std::str::Utf8Error> {
    let nul_range_end = utf8_src
        .iter()
        .position(|&c| c == b'\0')
        .unwrap_or(utf8_src.len()); // default to length if no `\0` present
    ::std::str::from_utf8(&utf8_src[0..nul_range_end])
}
