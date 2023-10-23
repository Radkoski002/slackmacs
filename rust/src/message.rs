use std::thread;

use emacs::{defun, Env, Result};

use crate::api_helpers::get_data;
use crate::api_types::messages::BaseMessage;
use crate::custom_errors::api_error;
use crate::helpers::handle_long_operation::handle_long_operation;
use crate::helpers::url_builder::ApiPaths;

#[defun(user_ptr)]
fn send_text(
    token: String,
    cookie: String,
    message: String,
    channel: String,
    env: &Env,
) -> Result<BaseMessage> {
    let (tx, rx) = std::sync::mpsc::channel::<i32>();
    let params = format!("text={}&channel={}", message, channel);
    let handle = thread::spawn(move || {
        let data = get_data::<BaseMessage>(
            cookie,
            token,
            ApiPaths::SendMessage,
            "message".to_string(),
            Some(params),
        );
        tx.send(1).unwrap();
        data
    });
    handle_long_operation(env, rx, Some(String::from("Sending message")))?;
    let message_data = handle.join().unwrap();
    match message_data {
        Ok(data) => return Ok(data),
        Err(err) => return env.signal(api_error, (err.message,))?,
    }
}
