use std::{collections::HashMap, time::Duration};

use super::mpmc::{recv_shared, SharedReceiver};
use super::repr::{Program, PromptMessage};
use reqwest::Client;
use tokio::{sync::mpsc::Sender, task::JoinSet};
use url::Url;

#[derive(Debug)]
enum ComplError {
    RequestError(String),
    StatusError(String),
    SerializationError(String),
}

pub async fn handshake(server_url: &Url) {
    let client = reqwest::Client::new();
    let info_endpoint = server_url.join("info").expect("Info endpoint should succeed");
    loop { 
        match client.get(info_endpoint.clone()).send().await { 
            Ok(_) => return,
            Err(_) => tokio::time::sleep(Duration::from_secs(5)).await
        }
    }
}

pub async fn spawn_connections(
    num_connections: usize,
    compl_queue: SharedReceiver<Box<Program>>,
    run_queue: Sender<Box<Program>>,
    log_queue: Sender<(String, Option<String>)>,
    server_url: Url,
) -> () {
    let mut tasks = JoinSet::new();
    let endpoint_url = server_url.join("generate").expect("Url should succeed");
    for _ in 0..num_connections {
        let cq = compl_queue.clone();
        let rq = run_queue.clone();
        let lq = log_queue.clone();
        tasks.spawn(make_completion_requests(cq, rq, lq, endpoint_url.clone()));
    }
    tasks.detach_all()
}
async fn make_single_request(
    prog: &Program,
    client: &Client,
    endpoint_url: Url,
) -> Result<String, ComplError> {
    let msg = serde_json::to_string(&PromptMessage::from(prog.clone()))
        .expect("Message should serialize");
    let resp = client
        .post(endpoint_url)
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
    log_queue: Sender<(String, Option<String>)>,
    endpoint_url: Url,
) -> () {
    let client = reqwest::Client::new();
    loop {
        let mut prog: Box<Program> = match recv_shared(compl_queue.clone()).await {
            None => {
                let _ = log_queue
                    .send(("Early return from completions".to_string(), None))
                    .await;
                return;
            }
            Some(p) => p,
        };
        let mut attempts = 0;
        while attempts < 10 {
            match make_single_request(&prog, &client, endpoint_url.clone()).await {
                Ok(compl) => {
                    (*prog).completion = compl;
                    let _ = run_queue.send(prog).await;
                    break;
                }
                Err(e) => {
                    attempts += 1;
                    let _ = log_queue
                        .send((
                            format!(
                                "Faiiled with error: {:?}, Attempts remaining: {}",
                                e,
                                10 - attempts
                            ),
                            None,
                        ))
                        .await;
                    tokio::time::sleep(Duration::from_secs(5)).await
                }
            }
        }
    }
}
