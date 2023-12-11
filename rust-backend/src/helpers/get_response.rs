use http_body_util::combinators::BoxBody;
use hyper::body::Bytes;
use hyper::{HeaderMap, Response, StatusCode};

use super::{
    responses::full,
    url_builder::{get_url, ApiPaths},
};

use std::io::{Error, ErrorKind};

pub fn check_headers(headers: &HeaderMap) -> Result<(String, String), Error> {
    let slack_token = match headers.get("token") {
        Some(token) => token.to_str().unwrap().to_string(),
        None => return Err(Error::new(ErrorKind::NotFound, "No token provided")),
    };

    let slack_cookie = match headers.get("cookie") {
        Some(cookie) => cookie.to_str().unwrap().to_string(),
        None => return Err(Error::new(ErrorKind::NotFound, "No cookie provided")),
    };
    return Ok((slack_token, slack_cookie));
}

async fn fetch_api(
    client: reqwest::Client,
    token: String,
    cookie: String,
    path: ApiPaths,
    options: Option<String>,
) -> serde_json::Value {
    let res = client
        .post(&get_url(&token, path, options))
        .header("Cookie", format!("d={}", cookie))
        .send()
        .await
        .unwrap();

    let json = res.json::<serde_json::Value>().await.unwrap();
    match json.get("error") {
        Some(e) => println!("Error: {}", e),
        None => (),
    }
    json
}

pub async fn get_raw_fetch_result(
    client: reqwest::Client,
    headers: &HeaderMap,
    path: ApiPaths,
    options: Option<String>,
) -> String {
    let (slack_token, slack_cookie) = match check_headers(headers) {
        Ok((token, cookie)) => (token, cookie),
        Err(err) => return err.to_string(),
    };
    let res = fetch_api(client, slack_token, slack_cookie, path, options)
        .await
        .to_string();
    res
}

pub async fn get_response(
    client: reqwest::Client,
    headers: &HeaderMap,
    path: ApiPaths,
    options: Option<String>,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, hyper::Error> {
    let (slack_token, slack_cookie) = match check_headers(headers) {
        Ok((token, cookie)) => (token, cookie),
        Err(err) => {
            let mut response = Response::new(full(err.to_string()));
            *response.status_mut() = StatusCode::BAD_REQUEST;
            return Ok(response);
        }
    };
    let res = fetch_api(client, slack_token, slack_cookie, path, options)
        .await
        .to_string();
    Ok(Response::new(full(res)))
}
