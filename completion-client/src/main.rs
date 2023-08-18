mod completions;
mod logging;
mod mpmc;
mod reader;
mod repr;
mod runner;
mod writer;

use std::{path::PathBuf, sync::Arc, time::Duration};

use clap::Parser;
use repr::Program;
use tokio::{
    join, spawn,
    sync::{mpsc::channel, Mutex},
};
use url::Url;

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
    server_url: String,

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
        server_url,
        num_connections,
        attempt_limit,
        num_runners,
        log_file,
    } = Cli::parse();
    let server_url: Url = Url::parse(&server_url).expect("Server URL should be valid");
    let _ = tokio::time::timeout(Duration::from_secs(60), completions::handshake(&server_url)).await.unwrap();
    let seen_ids = reader::read_output_jsonl(&output_file);
    let channel_size = 2 * usize::max(num_connections, num_runners);  
    let (readtoks_send, readtoks_recv) = channel::<()>(channel_size);
    for _ in 0..num_connections { 
        let _ = readtoks_send.send(()).await;
    }
    let (complq_send, complq_recv) = channel::<Box<Program>>(channel_size);
    let (runq_send, runq_recv) = channel::<Box<Program>>(channel_size);
    let (finq_send, finq_recv) = channel::<Box<Program>>(channel_size);
    let (logq_send, logq_recv) = channel::<(String, Option<String>)>(channel_size);
    let rd_hdl = spawn(reader::read_input_jsonl(
        prompt_file,
        readtoks_recv,
        complq_send.clone(),
        logq_send.clone(), 
        seen_ids 
    ));
    let c_hdl = spawn(completions::spawn_connections(
        num_connections,
        Arc::new(Mutex::new(complq_recv)),
        runq_send.clone(),
        logq_send.clone(),
        server_url,
    ));
    let rn_hdl = spawn(runner::spawn_runners(
        Arc::new(Mutex::new(runq_recv)),
        complq_send.clone(),
        finq_send.clone(),
        logq_send.clone(),
        readtoks_send.clone(),
        attempt_limit,
        num_runners,
    ));
    let w_hdl = spawn(writer::write_jsonl(output_file, finq_recv));
    let l_hdl = spawn(logging::logger(logq_recv, log_file));
    let _ = join!(rd_hdl, c_hdl, rn_hdl, w_hdl, l_hdl);
}
