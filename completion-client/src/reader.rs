use std::collections::HashSet;
use std::path::PathBuf;

use tokio::fs::File;
use tokio::sync::mpsc::{Sender, Receiver};
use tokio::io::AsyncBufReadExt;
use std::io::BufRead;

use crate::repr::DatasetOutput;

use super::repr::{Program, Prompt};

pub async fn read_input_jsonl(
    prompt_file: PathBuf,
    mut read_toks: Receiver<()>,
    compl_queue: Sender<Box<Program>>,
    log_queue: Sender<(String, Option<String>)>,
    seen_ids: HashSet<String>,
) {
    let f = File::open(prompt_file)
        .await
        .expect("Prompt file should exist");
    let f = tokio::io::BufReader::new(f);
    let mut lines = f.lines();
    while let Some(()) = read_toks.recv().await {
        loop {
            if let Some(line) = lines.next_line().await.unwrap() { 
                let prompt: Prompt = serde_json::from_str(&line).expect("Should be valid prompt");
                let id = get_id_from_path(prompt.original.to_string());
                if !seen_ids.contains(&id) {
                    let prog = Program::from(prompt);
                    let _ = compl_queue.send(Box::new(prog)).await;
                    break;
                } else {
                    let _ = log_queue.send((format!("Ignoring prompt: {}", id), None)).await;
                }
            }
        }
        
    }
    let _ = log_queue.send((String::from("Read all prompts"), None)).await;
}

pub fn read_output_jsonl(out_file: &PathBuf) -> HashSet<String> {
    let mut seen = HashSet::new();
    if let Ok(f) = std::fs::File::open(out_file) {
        let reader = std::io::BufReader::new(f);
        for line in reader.lines() {
            let ds: DatasetOutput =
                serde_json::from_str(&line.unwrap()).expect("Should be valid existing completion");
            let id = get_id_from_path(ds.path);
            let _ = seen.insert(id);
        }
    }
    seen
}

fn get_id_from_path(path_str: String) -> String {
    path_str
        .split("/")
        .last()
        .expect("Path should split")
        .split("_")
        .nth(1)
        .expect("Path should have id")
        .to_string()
}
