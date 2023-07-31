use std::{collections::HashMap, time::Duration};

use super::repr::{Program, PromptMessage};
use super::mpmc::{SharedReceiver, recv_shared};
use reqwest::Client;
use tokio::{
    sync::mpsc::Sender,
    task::JoinSet,
};

#[derive(Debug)]
enum ComplError {
    RequestError(String),
    StatusError(String),
    SerializationError(String),
}

pub async fn spawn_connections(
    num_connections: usize,
    compl_queue: SharedReceiver<Box<Program>>,
    run_queue: Sender<Box<Program>>,
    server_url: &'static str,
) -> () {
    let mut tasks = JoinSet::new();
    for _ in 0..num_connections {
        let cq = compl_queue.clone();
        let rq = run_queue.clone();
        tasks.spawn(make_completion_requests(cq, rq, server_url)) ;
    }
    tasks.detach_all()
}
async fn make_single_request(
    prog: &Program,
    client: &Client,
    server_url: &'static str,
) -> Result<String, ComplError> {
    let msg = serde_json::to_string(&PromptMessage::from(prog.clone()))
        .expect("Message should serialize");
    let resp = client
        .post(server_url)
        .body(msg.clone())
        .header("Content-Type", "application/json")
        .send()
        .await
        .map_err(|e| ComplError::RequestError(format!("{:?}, sent msg: {}", e, msg)))?;
    let resp_json = resp
        .error_for_status()
        .map_err(|e| ComplError::StatusError(format!("{:?}", e)))?
        .json::<HashMap<String, String>>()
        .await
        .map_err(|e| ComplError::SerializationError(format!("{:?}", e)))?;
    Ok(resp_json["generated_text"].to_string())
}
async fn make_completion_requests(
    compl_queue: SharedReceiver<Box<Program>>,
    run_queue: Sender<Box<Program>>,
    server_url: &'static str,
) -> () {
    let client = reqwest::Client::new();
    loop {
        let mut prog: Box<Program> = match recv_shared(compl_queue.clone()).await { 
            None => { println!("Early return from completions"); return}
            Some(p) => p
        }; 
        let mut attempts = 0;
        while attempts < 10 {
            match make_single_request(&prog, &client, server_url).await {
                Ok(compl) => {
                    (*prog).completion = compl;
                    let _ = run_queue.send(prog).await;
                    break;
                }
                Err(e) => {
                    attempts += 1;
                    println!(
                        "Faiiled with error: {:?}, Attempts remaining: {}",
                        e,
                        10 - attempts
                    );
                    tokio::time::sleep(Duration::from_secs(5)).await
                }
            }
        }
    }
}
