use std::sync::Arc;

use tokio::sync::{mpsc::Receiver, Mutex};

pub type SharedReceiver<T> = Arc<Mutex<Receiver<T>>>;

pub async fn recv_shared<T>(shrec: SharedReceiver<T>) -> Option<T> {
    shrec.lock().await.recv().await
}
