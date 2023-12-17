use emacs::{defun, Env, Result, Value};

use crate::{api_types::Conversation, custom_errors::api_error, helpers::get_vector_from_json};

#[defun]
fn get_messages<'a>(json: String, env: &Env) -> Result<Value> {
    let vector = get_vector_from_json(json, "messages".to_string(), env);
    match vector {
        Ok(vector) => {
            return Ok(env.list(&vector)?);
        }
        Err(error) => env.signal(api_error, (error.message,))?,
    }
}

#[defun]
fn get_messages_reversed<'a>(json: String, env: &Env) -> Result<Value> {
    let vector = get_vector_from_json(json, "messages".to_string(), env);
    match vector {
        Ok(mut vector) => {
            vector.reverse();
            return Ok(env.list(&vector)?);
        }
        Err(error) => env.signal(api_error, (error.message,))?,
    }
}

#[defun]
fn check_buffer_name(name: String, env: &Env) -> Result<bool> {
    if !["conversation".to_string(), "reply".to_string()].contains(&name) {
        return env.signal(api_error, ("Not a conversation buffer".to_string(),));
    }
    Ok(true)
}

#[defun]
fn get_id_from_buffer_name(buffer_name: String, env: &Env) -> Result<String> {
    let name_split: Vec<&str> = buffer_name.split("-").collect();
    if !["conversation", "reply"].contains(name_split.get(0).unwrap()) {
        return env.signal(api_error, ("Not a conversation buffer".to_string(),));
    }
    Ok(name_split.get(1).unwrap().to_string())
}

#[defun]
fn get_ts_from_buffer_name(buffer_name: String, env: &Env) -> Result<String> {
    let name_split: Vec<&str> = buffer_name.split("-").collect();
    if name_split.len() != 3 || !["reply"].contains(name_split.get(0).unwrap()) {
        return env.signal(api_error, ("Not a reply buffer".to_string(),));
    }
    Ok(name_split.get(2).unwrap().to_string())
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
