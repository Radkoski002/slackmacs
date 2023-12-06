use std::thread;

use emacs::{defun, Result, Value};
use websocket::header::{Cookie, Headers};
use websocket::{ClientBuilder, OwnedMessage};

use crate::helpers::fetch_api::fetch_api;
use crate::helpers::url_builder::ApiPaths;

#[defun]
fn open(token: String, cookie: String, lambda: Value) -> Result<()> {
    let (tx, rx) = std::sync::mpsc::channel::<String>();
    let mut my_headers = Headers::new();
    let cookie_clone = cookie.clone();
    my_headers.set(Cookie(vec![format!("d={}", cookie_clone)]));
    let client = reqwest::blocking::Client::new();
    let json = fetch_api(client, &token, &cookie, ApiPaths::Websocket, None);
    let url = json.get("url").unwrap().to_string();
    let formatted_url = format!("{}?token={}", url, token).replace("\"", "");
    let mut client = ClientBuilder::new(&formatted_url)
        .unwrap()
        .custom_headers(&my_headers)
        .connect(None)
        .unwrap();
    thread::spawn(move || {
        for message in client.incoming_messages() {
            let message = message.unwrap();
            match message {
                OwnedMessage::Text(text) => {
                    tx.send(text).unwrap();
                }
                _ => (),
            }
        }
    });
    loop {
        let message = rx.try_recv();
        match message {
            Ok(message) => {
                lambda.call((message,))?;
            }
            Err(_) => {
                thread::sleep(std::time::Duration::from_millis(100));
                lambda.call(("waiting...",))?;
            }
        }
    }
}
