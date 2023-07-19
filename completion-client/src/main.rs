mod completions;
mod reader;
mod repr;
mod runner;
mod writer;

use std::{path::PathBuf, sync::Arc};

use clap::Parser;
use repr::Program;
use tokio::{sync::{mpsc::channel, Mutex}, spawn, join};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(long)]
    prompt_file: PathBuf,

    #[arg(long)]
    output_file: PathBuf,

    #[arg(long)]
    server_url: String,

    #[arg(long)]
    num_connections: usize,
}

#[tokio::main]
async fn main() {
    let Cli {
        prompt_file,
        output_file,
        server_url,
        num_connections,
    } = Cli::parse();
    let server_url :&'static str = Box::leak(server_url.into_boxed_str());
    let (complq_send, complq_recv) = channel::<Box<Program>>(10*num_connections);
    let (runq_send, runq_recv) = channel::<Box<Program>>(10*num_connections);
    let (finq_send, finq_recv) = channel::<Box<Program>>(10*num_connections);
    let rd_hdl = spawn(reader::read_jsonl(prompt_file, complq_send.clone()));
    let c_hdl = spawn(completions::spawn_connections(num_connections, Arc::new(Mutex::new(complq_recv)), runq_send.clone(), server_url));
    let rn_hdl = spawn(runner::prog_runner(runq_recv, complq_send.clone(), finq_send.clone()));
    let w_hdl = spawn(writer::write_jsonl(output_file, finq_recv));
    let _ = join!(rd_hdl, c_hdl, rn_hdl, w_hdl);
}
