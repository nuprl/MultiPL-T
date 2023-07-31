use std::error::Error;
use std::fmt::Debug;

use crate::repr::EvalResult;

use super::repr::Program;
use super::mpmc::{SharedReceiver, recv_shared};

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
    Fail
}
pub async fn spawn_runners(
    run_queue: SharedReceiver<Box<Program>>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
    attempt_limit: u32, 
    num_runners: usize,
) -> () {
    let mut tasks = JoinSet::new();
    for _ in 0..num_runners {
        let rq = run_queue.clone();
        let cq = compl_queue.clone();
        let fq = fin_queue.clone();
        tasks.spawn(run_programs(rq, cq, fq, attempt_limit));
    }
    tasks.detach_all()
}
async fn run_programs(
    run_queue: SharedReceiver<Box<Program>>,
    compl_queue: Sender<Box<Program>>,
    fin_queue: Sender<Box<Program>>,
    attempt_limit: u32,
) {
    loop {
        let mut prog: Box<Program> = match recv_shared(run_queue.clone()).await { 
            None => { println!("Early return from runners"); return}
            Some(p) => p
        };
        match run_single_program(&prog).await {
            Ok(RunRes::Succ) => fin_queue.send(prog).await.unwrap(),
            Ok(RunRes::Fail) => { 
                if let Some(()) = prog.inc_attempts(attempt_limit) { 
                    let _ = compl_queue.send(prog).await.unwrap();
                }
                else {
                    println!("No more attempts for {}", prog.name);
                }
            }
            Err(e) => { 
                if let Some(()) =  prog.inc_attempts(attempt_limit) {
                    compl_queue.send(prog).await.unwrap();
                }
                println!("{:?}", e)
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
        .map_err(|e| RunError(format!("Error running child: {:?}", e)))?;
    let out = child
        .wait_with_output()
        .await
        .map_err(|e| RunError::from(e))?;
    let res = serde_json::from_str::<EvalResult>(
        str_from_u8_nul_utf8(&out.stdout).map_err(|e| RunError(format!("Error parsing utf8: {:?}", e)))?,
    )
    .map_err(|e| RunError(format!("Serialization Error: {:?}", e)))?;
    if res.status.to_lowercase().as_str() == "ok" { 
        Ok(RunRes::Succ)
    }
    else { 
        Ok(RunRes::Fail)
    }
}


//https://stackoverflow.com/a/42067321
fn str_from_u8_nul_utf8(utf8_src: &[u8]) -> Result<&str, std::str::Utf8Error> {
    let nul_range_end = utf8_src.iter()
        .position(|&c| c == b'\0')
        .unwrap_or(utf8_src.len()); // default to length if no `\0` present
    ::std::str::from_utf8(&utf8_src[0..nul_range_end])
}