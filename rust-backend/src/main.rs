use std::net::SocketAddr;
use std::sync::Arc;

use helpers::get_response::{check_headers, get_response};
use helpers::url_builder::ApiPaths;
use http_body_util::{combinators::BoxBody, BodyExt};
use hyper;
use hyper::body::Bytes;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Method, StatusCode};
use hyper::{Request, Response};
use hyper_util::rt::TokioIo;
use reqwest::Client;
use tokio::net::TcpListener;
use tokio::sync::Mutex;
use websocket::OwnedMessage;

use crate::helpers::get_response::get_raw_fetch_result;
use crate::helpers::responses::{empty, full};

mod helpers;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    let listener = TcpListener::bind(addr).await?;
    let request_client = Client::new();
    let is_websocket_running = Arc::new(Mutex::new(false));
    let websocket_messages = Arc::new(Mutex::new(Vec::<serde_json::Value>::new()));

    loop {
        let (stream, _) = listener.accept().await?;
        let client = request_client.clone();
        let is_websocket_running = is_websocket_running.clone();
        let websocket_messages = websocket_messages.clone();

        let io = TokioIo::new(stream);

        tokio::task::spawn(async move {
            if let Err(err) = http1::Builder::new()
                .serve_connection(
                    io,
                    service_fn(|req| {
                        request_handler(
                            req,
                            client.clone(),
                            is_websocket_running.clone(),
                            websocket_messages.clone(),
                        )
                    }),
                )
                .await
            {
                println!("Error serving connection: {:?}", err);
            }
        });
    }
}

async fn send(
    req: Request<hyper::body::Incoming>,
    client: reqwest::Client,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    let (head, body) = req.into_parts();
    let headers = &head.headers;
    let collected_body = body.collect().await.unwrap().to_bytes();
    let string_body = std::str::from_utf8(&collected_body).unwrap().to_string();
    return get_response(
        client,
        headers,
        ApiPaths::SendMessage,
        Some(string_body), // Some(string_body),
    )
    .await;
}

async fn get_users_list(
    req: Request<hyper::body::Incoming>,
    client: reqwest::Client,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    let headers = req.headers();
    return get_response(client, headers, ApiPaths::UsersList, None).await;
}

async fn get_conversation_list(
    req: Request<hyper::body::Incoming>,
    client: reqwest::Client,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    let headers = req.headers();
    let conversation_types = "types=public_channel,private_channel,mpim,im".to_string();
    return get_response(
        client,
        headers,
        ApiPaths::ConversationList,
        Some(conversation_types),
    )
    .await;
}

async fn get_conversation_history(
    req: Request<hyper::body::Incoming>,
    client: reqwest::Client,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    let (head, body) = req.into_parts();
    let headers = &head.headers;
    let collected_body = body.collect().await.unwrap().to_bytes();
    let string_body = std::str::from_utf8(&collected_body).unwrap().to_string();
    return get_response(
        client,
        headers,
        ApiPaths::ConversationHistory,
        Some(string_body), // Some(string_body),
    )
    .await;
}

async fn test(
    req: Request<hyper::body::Incoming>,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    let (_, body) = req.into_parts();
    let collected_body = body.collect().await.unwrap().to_bytes();
    let string_body = std::str::from_utf8(&collected_body).unwrap().to_string();
    return Ok(Response::new(full(string_body)));
}

async fn start(
    req: Request<hyper::body::Incoming>,
    client: reqwest::Client,
    is_websocket_running: Arc<Mutex<bool>>,
    websocket_messages: Arc<Mutex<Vec<serde_json::Value>>>,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    let websocket_checker = is_websocket_running.clone();
    let mut is_websocket_running = is_websocket_running.lock().await;
    if *is_websocket_running {
        return Ok(Response::new(full("already running".to_string())));
    }
    let (head, _) = req.into_parts();
    let headers = &head.headers;
    let response = get_raw_fetch_result(client, headers, ApiPaths::Websocket, None).await;
    let (token, cookie) = check_headers(headers).unwrap();
    let json = serde_json::from_str::<serde_json::Value>(&response).unwrap();
    tokio::spawn(async move {
        let mut headers = websocket::header::Headers::new();
        headers.set(websocket::header::Cookie(vec![format!("d={}", cookie)]));
        let url = json["url"].as_str().unwrap();
        let formatted_url = format!("{}?token={}", url, token);
        let mut websocket_client = websocket::ClientBuilder::new(formatted_url.as_str())
            .unwrap()
            .custom_headers(&headers)
            .connect(None)
            .unwrap();
        while let Ok(msg) = websocket_client.recv_message() {
            match msg {
                OwnedMessage::Text(text) => {
                    println!("Received: {}", text);
                    let mut websocket_messages = websocket_messages.lock().await;
                    websocket_messages
                        .push(serde_json::from_str::<serde_json::Value>(&text).unwrap());
                }
                OwnedMessage::Binary(bin) => println!("Received: {:?}", bin),
                OwnedMessage::Close(_) => {
                    println!("Received close message");
                    break;
                }
                _ => (),
            }
        }
        websocket_client.shutdown().unwrap();
        let mut websocket_checker = websocket_checker.lock().await;
        *websocket_checker = false;
    });

    *is_websocket_running = true;
    return Ok(Response::new(full("started".to_string())));
}

async fn get_websocket_updates(
    is_websocket_running: Arc<Mutex<bool>>,
    websocket_messages: Arc<Mutex<Vec<serde_json::Value>>>,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    let is_websocket_running = is_websocket_running.lock().await;
    if !*is_websocket_running {
        return Ok(Response::new(full("Websocket is not running".to_string())));
    }
    drop(is_websocket_running);
    let mut websocket_messages = websocket_messages.lock().await;
    let mut messages = Vec::<serde_json::Value>::new();
    std::mem::swap(&mut messages, &mut *websocket_messages);
    let messages = serde_json::to_string(&messages).unwrap();
    return Ok(Response::new(full(messages)));
}

async fn delete_message(
    req: Request<hyper::body::Incoming>,
    client: reqwest::Client,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    let (head, body) = req.into_parts();
    let headers = &head.headers;
    let collected_body = body.collect().await.unwrap().to_bytes();
    let string_body = std::str::from_utf8(&collected_body).unwrap().to_string();
    return get_response(
        client,
        headers,
        ApiPaths::DeleteMessage,
        Some(string_body), // Some(string_body),
    )
    .await;
}

async fn edit_message(
    req: Request<hyper::body::Incoming>,
    client: reqwest::Client,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    let (head, body) = req.into_parts();
    let headers = &head.headers;
    let collected_body = body.collect().await.unwrap().to_bytes();
    let string_body = std::str::from_utf8(&collected_body).unwrap().to_string();
    return get_response(
        client,
        headers,
        ApiPaths::EditMessage,
        Some(string_body), // Some(string_body),
    )
    .await;
}

async fn request_handler(
    req: Request<hyper::body::Incoming>,
    client: reqwest::Client,
    is_websocket_running: Arc<Mutex<bool>>,
    websocket_messages: Arc<Mutex<Vec<serde_json::Value>>>,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    match (req.method(), req.uri().path()) {
        (&Method::GET, "/") => Ok(Response::new(full("Try POSTing data to /echo"))),
        (&Method::POST, "/users-list") => get_users_list(req, client).await,
        (&Method::POST, "/conversation-list") => get_conversation_list(req, client).await,
        (&Method::POST, "/conversation-history") => get_conversation_history(req, client).await,
        (&Method::POST, "/delete-message") => delete_message(req, client).await,
        (&Method::POST, "/edit-message") => edit_message(req, client).await,
        (&Method::POST, "/get-websocket-updates") => {
            get_websocket_updates(is_websocket_running, websocket_messages).await
        }
        (&Method::POST, "/start") => {
            start(req, client, is_websocket_running, websocket_messages).await
        }
        (&Method::POST, "/send-message") => send(req, client).await,
        (&Method::POST, "/test") => test(req).await,
        (&Method::POST, "/echo") => Ok(Response::new(req.into_body().boxed())),

        _ => {
            let mut not_found = Response::new(empty());
            *not_found.status_mut() = StatusCode::NOT_FOUND;
            Ok(not_found)
        }
    }
}
