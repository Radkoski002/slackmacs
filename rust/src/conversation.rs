use emacs::{defun, Env, Result, Value, Vector};

use crate::api_helpers::{get_data, parse_data_vec};
use crate::api_types::messages::{message_matcher, BaseMessage};
use crate::custom_errors::api_error;
use crate::helpers::handle_long_operation::handle_long_operation;
use crate::helpers::url_builder::ApiPaths;

type MessageVec = Vec<BaseMessage>;

#[defun(user_ptr)]
fn get(token: String, cookie: String, channel_id: String, env: &Env) -> Result<MessageVec> {
    let (tx, rx) = std::sync::mpsc::channel::<i32>();
    let channel_param = format!("channel={}", channel_id);
    let handle = std::thread::spawn(move || {
        let data = get_data::<MessageVec>(
            cookie,
            token,
            ApiPaths::ConversationsHistory,
            "messages".to_string(),
            Some(channel_param),
        );
        tx.send(1).unwrap();
        data
    });
    handle_long_operation(env, rx, Some(String::from("Fetching messages")))?;
    let messages_data = handle.join().unwrap();
    match messages_data {
        Ok(data) => return Ok(data),
        Err(err) => return env.signal(api_error, (err.message,))?,
    }
}

#[defun]
fn parse<'a>(messages: &MessageVec, params: Vector<'a>) -> Result<Value<'a>> {
    parse_data_vec(messages, params, message_matcher)
}
