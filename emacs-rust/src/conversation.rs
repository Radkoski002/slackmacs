use emacs::{defun, Env, Result, Value};

use crate::{api_types::Conversation, custom_errors::api_error, helpers::get_vector_from_json};

#[defun]
fn get_messages(json: String, env: &Env) -> Result<Value> {
    let vector = get_vector_from_json(json, "messages".to_string(), env);
    match vector {
        Ok(mut vector) => {
            vector.reverse();
            return Ok(env.list(&vector)?);
        }
        Err(error) => env.signal(api_error, (error.message,))?,
    }
}

#[defun(user_ptr)]
fn from_json(json: String) -> Result<Conversation> {
    let parsed_json = serde_json::from_str::<Conversation>(&json).unwrap();
    Ok(parsed_json)
}

#[defun]
fn get_id(conversation: &Conversation) -> Result<String> {
    Ok(conversation.get_id())
}

#[defun]
fn get_name(conversation: &Conversation) -> Result<String> {
    Ok(conversation.get_name())
}

#[defun]
fn get_user_id(conversation: &Conversation) -> Result<String> {
    Ok(conversation.get_user())
}
