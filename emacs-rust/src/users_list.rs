use emacs::{defun, Env, IntoLisp, Result, Value};

use crate::api_types::user::User;
use crate::custom_errors::api_error;

#[defun]
fn parse_from_json_string(json: String, env: &Env) -> Result<Value> {
    let parsed_json = serde_json::from_str::<serde_json::Value>(&json).unwrap();
    let status = parsed_json.get("ok").unwrap().to_string();
    if status != "true" {
        let error_message = parsed_json.get("error").unwrap().to_string();
        return env.signal(api_error, (error_message,))?;
    }
    let messages = parsed_json.get("members").unwrap().as_array().unwrap();
    let mut final_vec = vec![];
    for message in messages {
        // env.message(message.to_string())?;
        final_vec.push(message.to_string().into_lisp(env)?)
    }
    final_vec.reverse();
    Ok(env.list(&final_vec)?)
}

#[defun(user_ptr)]
fn parse_string_to_user(json: String) -> Result<User> {
    let parsed_json = serde_json::from_str::<User>(&json).unwrap();
    Ok(parsed_json)
}

#[defun]
fn get_user_id(user: &User) -> Result<String> {
    Ok(user.get_id())
}

#[defun]
fn get_user_name(user: &User) -> Result<String> {
    Ok(user.get_name())
}
