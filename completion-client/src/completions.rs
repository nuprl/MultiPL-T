use std::{collections::HashMap, sync::Arc};

use super::repr::{Program, PromptMessage};
use tokio::{
    sync::{
        mpsc::{Receiver, Sender},
        Mutex,
    },
    task::JoinSet,
};

pub async fn spawn_connections(
    num_connections: usize,
    compl_queue: Arc<Mutex<Receiver<Box<Program>>>>,
    run_queue: Sender<Box<Program>>,
    server_url: &'static str,
) -> () {
    let mut tasks = JoinSet::new();
    for _ in 0..num_connections {
        let cq = compl_queue.clone();
        let rq = run_queue.clone();
        tasks.spawn(make_completion_requests(cq, rq, server_url));
    }
    while let Some(_) = tasks.join_next().await {
        ()
    }
}
async fn make_completion_requests(
    compl_queue: Arc<Mutex<Receiver<Box<Program>>>>,
    run_queue: Sender<Box<Program>>,
    server_url: &'static str,
) -> () {
    let client = reqwest::Client::new();
    loop {
        let mut prog: Box<Program> = {
            let mut chan = compl_queue.clone().lock_owned().await;
            match chan.recv().await {
                None => return,
                Some(p) => p,
            }
        };
        let msg = serde_json::to_string(&PromptMessage::from(*prog.clone()))
            .expect("Message should serialize");
        let completion = match client
            .post(server_url)
            .body(msg.clone())
            .header("Content-Type", "application/json")
            .send()
            .await
        {
            Ok(resp) => {
                if resp.status() == 200 {
                    resp.json::<HashMap<String, String>>().await.unwrap()["generated_text"].clone()
                } else {
                    let _ = dbg!(resp.status());
                    let _ = dbg!(msg);
                    let _ = dbg!(resp.json::<HashMap<String, String>>().await.unwrap());
                    panic!("Did not return a 200 response")
                }
            }
            Err(e) => panic!("{}", e),
        };
        (*prog).completion = completion;
        let _ = run_queue.send(prog).await;
    }
}
