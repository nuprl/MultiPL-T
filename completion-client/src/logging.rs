use std::path::PathBuf;

use tokio::{
    fs::OpenOptions,
    io::{AsyncWriteExt, BufWriter},
    sync::mpsc::Receiver,
};

use crate::repr::Program;

pub async fn logger(mut log_queue: Receiver<(String, Option<String>)>, log_file: PathBuf) {
    let f = OpenOptions::new()
        .write(true)
        .append(true)
        .open(log_file)
        .await
        .expect("Log file open should succeed");
    let mut f = BufWriter::new(f);
    while let Some((lmsg, opstr)) = log_queue.recv().await {
        let _ = f.write_all(format!("{}\n", lmsg).as_bytes()).await;
        if let Some(pstr) = opstr {
            let _ = f.write_all(format!("{}\n----\n", pstr).as_bytes()).await;
        }
    }
}