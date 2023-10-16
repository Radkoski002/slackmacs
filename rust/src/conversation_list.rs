use std::thread;

use emacs::{defun, Env, Result, Value, Vector};

use crate::api_helpers::{get_data, parse_data_vec};

use crate::api_types::conversation::{conversation_matcher, Conversation};
use crate::custom_errors::api_error;
use crate::helpers::{handle_long_operation::handle_long_operation, url_builder::ApiPaths};

type ConversationVec = Vec<Conversation>;

#[defun(user_ptr)]
fn get(token: String, cookie: String, env: &Env) -> Result<ConversationVec> {
    let (tx, rx) = std::sync::mpsc::channel::<i32>();
    let conversation_types = "types=public_channel,private_channel,mpim,im".to_string();
    let handle = thread::spawn(move || {
        let data = get_data::<ConversationVec>(
            cookie,
            token,
            ApiPaths::ConversationList,
            "channels".to_string(),
            Some(conversation_types),
        );
        tx.send(1).unwrap();
        data
    });
    handle_long_operation(env, rx, Some(String::from("Fetching conversations")))?;
    let conversation_data = handle.join().unwrap();
    match conversation_data {
        Ok(data) => return Ok(data),
        Err(err) => return env.signal(api_error, (err.message,))?,
    }
}

#[defun]
fn parse<'a>(channels: &ConversationVec, params: Vector<'a>) -> Result<Value<'a>> {
    parse_data_vec(channels, params, conversation_matcher)
}
