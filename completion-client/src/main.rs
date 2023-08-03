mod completions;
mod logging;
mod mpmc;
mod reader;
mod repr;
mod runner;
mod writer;

use std::{path::PathBuf, sync::Arc};

use clap::Parser;
use repr::Program;
use tokio::{
    join, spawn,
    sync::{mpsc::channel, Mutex},
};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(long)]
    prompt_file: PathBuf,

    #[arg(long)]
    output_file: PathBuf,

    #[arg(long)]
    log_file: PathBuf,

    #[arg(long)]
    endpoint_url: String,

    #[arg(long)]
    num_connections: usize,

    #[arg(long)]
    attempt_limit: u32,

    #[arg(long)]
    num_runners: usize,
}

#[tokio::main]
async fn main() {
    let Cli {
        prompt_file,
        output_file,
        endpoint_url,
        num_connections,
        attempt_limit,
        num_runners,
        log_file,
    } = Cli::parse();
    let endpoint_url: &'static str = Box::leak(endpoint_url.into_boxed_str());
    let seen_ids = reader::read_output_jsonl(&output_file).await;
    let (complq_send, complq_recv) = channel::<Box<Program>>(100 + num_connections);
    let (runq_send, runq_recv) = channel::<Box<Program>>(100 + num_connections);
    let (finq_send, finq_recv) = channel::<Box<Program>>(100 + num_connections);
    let (logq_send, logq_recv) =
        channel::<(String, Option<String>)>(100 + num_connections + num_runners);
    let rd_hdl = spawn(reader::read_input_jsonl(
        prompt_file,
        complq_send.clone(),
        logq_send.clone(), 
        seen_ids 
    ));
    let c_hdl = spawn(completions::spawn_connections(
        num_connections,
        Arc::new(Mutex::new(complq_recv)),
        runq_send.clone(),
        logq_send.clone(),
        endpoint_url,
    ));
    let rn_hdl = spawn(runner::spawn_runners(
        Arc::new(Mutex::new(runq_recv)),
        complq_send.clone(),
        finq_send.clone(),
        logq_send.clone(),
        attempt_limit,
        num_runners,
    ));
    let w_hdl = spawn(writer::write_jsonl(output_file, finq_recv));
    let l_hdl = spawn(logging::logger(logq_recv, log_file));
    let _ = join!(rd_hdl, c_hdl, rn_hdl, w_hdl, l_hdl);
}
