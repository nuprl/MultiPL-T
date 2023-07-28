use std::collections::HashSet;
use std::path::PathBuf;

use tokio::fs::File;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::sync::mpsc::Sender;

use crate::repr::DatasetOutput;

use super::repr::{Program, Prompt};

pub async fn read_input_jsonl(
    prompt_file: PathBuf, 
    compl_queue: Sender<Box<Program>>,
    seen_ids: HashSet<String>
) {
    let f = File::open(prompt_file)
        .await
        .expect("Prompt file should exist");
    let f = BufReader::new(f);
    let mut lines = f.lines();
    while let Some(line) = lines.next_line().await.unwrap() {
        let prompt: Prompt = serde_json::from_str(&line).expect("Should be valid prompt");
        let id = get_id_from_path(prompt.original.to_string()); 
        if !seen_ids.contains(&id) { 
            let prog = Program::from(prompt);
            compl_queue.send(Box::new(prog)).await.unwrap()
        }
        else { 
            let _ = println!("Ingnoring prompt: {}", id);
        }
    }
    println!("Read all prompts")
}

pub async fn read_output_jsonl(out_file: &PathBuf) -> HashSet<String> {
    let mut seen = HashSet::new();
    match File::open(out_file).await { 
        Ok(f) => { 
            let f = BufReader::new(f);
            let mut lines = f.lines();
            while let Some(line) = lines.next_line().await.unwrap() {
                let ds: DatasetOutput = serde_json::from_str(&line).expect("Should be valid existing completion");
                let id = get_id_from_path(ds.path);
                let _ = seen.insert(id);
            }
        }
        Err(_) => { 
            File::create(out_file)
                .await
                .expect("File creation should succeed");
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
