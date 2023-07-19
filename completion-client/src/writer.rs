use std::path::PathBuf;

use tokio::{
    fs::File,
    io::{AsyncWriteExt, BufWriter},
    sync::mpsc::Receiver,
};

use crate::repr::DatasetOutput;

use super::repr::Program;

pub async fn write_jsonl(out_file: PathBuf, mut fin_queue: Receiver<Box<Program>>) {
    let f = File::create(out_file)
        .await
        .expect("File creation should succeed");
    let mut f = BufWriter::new(f);
    while let Some(prog) = fin_queue.recv().await {
        let output = DatasetOutput::from(*prog);
        let output_str = serde_json::to_string(&output).unwrap();
        let _ = f.write(output_str.as_bytes());
    }
}
