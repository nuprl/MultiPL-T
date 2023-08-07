use std::path::PathBuf;

use tokio::{
    fs::OpenOptions,
    io::{AsyncWriteExt, BufWriter},
    sync::mpsc::Receiver,
};

use crate::repr::DatasetOutput;

use super::repr::Program;

pub async fn write_jsonl(out_file: PathBuf, mut fin_queue: Receiver<Box<Program>>) {
    let f = OpenOptions::new()
        .write(true)
        .append(true)
        .create(true)
        .open(out_file)
        .await
        .expect("Out file open should succeed");
    let mut f = BufWriter::new(f);
    while let Some(prog) = fin_queue.recv().await {
        let output = DatasetOutput::from(*prog);
        let output_str = serde_json::to_string(&output).unwrap();
        let _ = f.write_all(format!("{}\n", output_str).as_bytes()).await;
        let _ = f.flush().await;
    }
}
