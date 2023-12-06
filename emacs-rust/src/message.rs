use emacs::{defun, Result};

use crate::api_types::BaseMessage;

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
