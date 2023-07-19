use std::path::PathBuf;

use tokio::fs::File;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::sync::mpsc::Sender;

use super::repr::{Program, Prompt};

pub async fn read_jsonl(prompt_file: PathBuf, compl_queue: Sender<Box<Program>>) {
    let f = File::open(prompt_file)
        .await
        .expect("Prompt file should exist");
    let f = BufReader::new(f);
    let mut lines = f.lines();
    while let Some(line) = lines.next_line().await.unwrap() {
        let prompt: Prompt = serde_json::from_str(&line).expect("Should be valid prompt");
        let prog = Program::from(prompt);
        compl_queue.send(Box::new(prog)).await.unwrap()
    }
}
