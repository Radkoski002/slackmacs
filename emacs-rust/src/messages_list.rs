use std::collections::HashMap;

use emacs::{defun, Env, Result, Value};

use crate::{
    api_types::{BaseMessage, ReplyMessage, Slack},
    custom_errors::api_error,
    helpers::get_rust_vector_from_json,
};
use itertools::Itertools;

#[defun]
fn from_json(
    json: String,
    conversation_id: String,
    slack_instance: &mut Slack,
    env: &Env,
) -> Result<()> {
    let json_clone = json.clone();
    let message_vec = get_rust_vector_from_json::<BaseMessage>(json_clone, "messages".to_string());
    match message_vec {
        Ok(rust_vector) => {
            let conversation = slack_instance
                .conversations
                .get_mut(conversation_id.as_str())
                .unwrap();
            let messages = match conversation.messages.as_mut() {
                Some(_) => return Ok(()),
                None => {
                    conversation.messages = Some(HashMap::new());
                    conversation.messages.as_mut().unwrap()
                }
            };
            for message in rust_vector.iter() {
                messages.insert(message.get_ts(), message.clone());
            }
        }
        Err(error) => env.signal(api_error, (error.message,))?,
    }
    Ok(())
}

#[defun]
fn replies_from_json(
    json: String,
    conversation_id: String,
    parent_ts: String,
    slack_instance: &mut Slack,
    env: &Env,
) -> Result<()> {
    let json_clone = json.clone();
    let reply_vec = get_rust_vector_from_json::<ReplyMessage>(json_clone, "messages".to_string());
    match reply_vec {
        Ok(reply_vector) => {
            let conversation = slack_instance
                .conversations
                .get_mut(conversation_id.as_str())
                .unwrap();
            let parent_message = conversation
                .messages
                .as_mut()
                .unwrap()
                .get_mut(parent_ts.as_str())
                .unwrap();
            let replies = match parent_message.replies.as_mut() {
                Some(replies) => replies,
                None => {
                    parent_message.replies = Some(HashMap::new());
                    parent_message.replies.as_mut().unwrap()
                }
            };
            for reply in reply_vector.iter() {
                replies.insert(reply.get_ts(), reply.clone());
            }
        }
        Err(error) => env.signal(api_error, (error.message,))?,
    }
    Ok(())
}

#[defun]
fn create_message_buttons(
    slack_instance: &Slack,
    conversation_id: String,
    create_callback: Value,
) -> Result<()> {
    let conversation = slack_instance
        .conversations
        .get(conversation_id.as_str())
        .unwrap();
    let messages = conversation.messages.as_ref().unwrap();
    for key in messages.keys().sorted() {
        let message = messages.get(key).unwrap();
        let sender = match slack_instance.users.get(message.get_user().as_str()) {
            Some(user) => user.get_name(),
            None => message.get_user(),
        };
        let text = message.get_text();
        let ts = message.get_ts();
        let reply_count = message.get_reply_count();
        create_callback.call((sender, text, ts, reply_count))?;
    }
    Ok(())
}

#[defun]
fn create_reply_buttons(
    slack_instance: &Slack,
    conversation_id: String,
    parent_ts: String,
    create_callback: Value,
) -> Result<()> {
    let conversation = slack_instance
        .conversations
        .get(conversation_id.as_str())
        .unwrap();
    let messages = conversation.messages.as_ref().unwrap();
    let parent_message = messages.get(parent_ts.as_str()).unwrap();
    let replies = parent_message.replies.as_ref().unwrap();
    for key in replies.keys().sorted() {
        let message = replies.get(key).unwrap();
        let sender = match slack_instance.users.get(message.get_user().as_str()) {
            Some(user) => user.get_name(),
            None => message.get_user(),
        };
        let text = message.get_text();
        let ts = message.get_ts();
        create_callback.call((sender, text, ts))?;
    }
    Ok(())
}
