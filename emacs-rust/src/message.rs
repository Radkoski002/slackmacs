use emacs::{defun, Env, Result};

use crate::{
    api_types::{BaseMessage, ReplyMessage, Slack},
    custom_errors::api_error,
    helpers::get_value_from_json,
};

#[defun(user_ptr)]
fn from_json(json: String) -> Result<BaseMessage> {
    let parsed_json = serde_json::from_str::<BaseMessage>(&json).unwrap();
    Ok(parsed_json)
}

#[defun]
fn get_text(message: &BaseMessage) -> Result<String> {
    Ok(message.get_text())
}

#[defun]
fn get_sender(message: &BaseMessage) -> Result<String> {
    Ok(message.get_user())
}

#[defun]
fn get_ts(message: &BaseMessage) -> Result<String> {
    Ok(message.get_ts())
}

#[defun]
fn get_reply_count(message: &BaseMessage) -> Result<i16> {
    Ok(message.get_reply_count())
}

#[defun]
fn add(
    slack_instance: &mut Slack,
    reply_json: String,
    conversation_id: String,
    env: &Env,
) -> Result<()> {
    let message = get_value_from_json::<BaseMessage>(reply_json, "message".to_string());
    match message {
        Ok(message) => {
            let conversation = slack_instance
                .conversations
                .get_mut(conversation_id.as_str())
                .unwrap();
            let messages = conversation.messages.as_mut().unwrap();
            messages.insert(message.get_ts(), message);
        }
        Err(error) => return env.signal(api_error, (error.message,))?,
    }
    Ok(())
}

#[defun]
fn reply_add(
    slack_instance: &mut Slack,
    reply_json: String,
    conversation_id: String,
    parent_ts: String,
    env: &Env,
) -> Result<()> {
    let reply = get_value_from_json::<ReplyMessage>(reply_json, "message".to_string());
    match reply {
        Ok(message) => {
            let conversation = slack_instance
                .conversations
                .get_mut(conversation_id.as_str())
                .unwrap();
            let messages = conversation.messages.as_mut().unwrap();
            let parent_message = messages.get_mut(parent_ts.as_str()).unwrap();
            let replies = parent_message.replies.as_mut().unwrap();
            replies.insert(message.get_ts(), message);
        }
        Err(error) => return env.signal(api_error, (error.message,))?,
    }
    Ok(())
}

#[defun]
fn edit(
    slack_instance: &mut Slack,
    text: String,
    conversation_id: String,
    ts: String,
) -> Result<()> {
    let conversation = slack_instance
        .conversations
        .get_mut(conversation_id.as_str())
        .unwrap();
    let messages = conversation.messages.as_mut().unwrap();
    let message = messages.get_mut(ts.as_str()).unwrap();
    message.text = Some(text);
    Ok(())
}

#[defun]
fn reply_edit(
    slack_instance: &mut Slack,
    text: String,
    conversation_id: String,
    ts: String,
    parent_ts: String,
) -> Result<()> {
    let conversation = slack_instance
        .conversations
        .get_mut(conversation_id.as_str())
        .unwrap();
    let messages = conversation.messages.as_mut().unwrap();
    let parent_message = messages.get_mut(parent_ts.as_str()).unwrap();
    let replies = parent_message.replies.as_mut().unwrap();
    let reply = replies.get_mut(ts.as_str()).unwrap();
    reply.text = Some(text);
    Ok(())
}

#[defun]
fn delete(slack_instance: &mut Slack, conversation_id: String, ts: String) -> Result<()> {
    let conversation = slack_instance
        .conversations
        .get_mut(conversation_id.as_str())
        .unwrap();
    let messages = conversation.messages.as_mut().unwrap();
    messages.remove(ts.as_str());
    Ok(())
}

#[defun]
fn reply_delete(
    slack_instance: &mut Slack,
    conversation_id: String,
    ts: String,
    parent_ts: String,
) -> Result<()> {
    let conversation = slack_instance
        .conversations
        .get_mut(conversation_id.as_str())
        .unwrap();
    let messages = conversation.messages.as_mut().unwrap();
    let parent_message = messages.get_mut(parent_ts.as_str()).unwrap();
    let replies = parent_message.replies.as_mut().unwrap();
    replies.remove(ts.as_str());
    Ok(())
}
