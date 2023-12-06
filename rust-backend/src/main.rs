use std::net::SocketAddr;

use helpers::get_response::get_response;
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

use crate::helpers::responses::{empty, full};

mod helpers;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    let listener = TcpListener::bind(addr).await?;
    let client = Client::new();

    loop {
        let (stream, _) = listener.accept().await?;
        let client = client.clone();

        let io = TokioIo::new(stream);

        tokio::task::spawn(async move {
            if let Err(err) = http1::Builder::new()
                .serve_connection(io, service_fn(|req| request_handler(req, client.clone())))
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

async fn request_handler(
    req: Request<hyper::body::Incoming>,
    client: reqwest::Client,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    match (req.method(), req.uri().path()) {
        (&Method::GET, "/") => Ok(Response::new(full("Try POSTing data to /echo"))),
        (&Method::POST, "/users-list") => get_users_list(req, client).await,
        (&Method::POST, "/conversation-list") => get_conversation_list(req, client).await,
        (&Method::POST, "/conversation-history") => get_conversation_history(req, client).await,
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
