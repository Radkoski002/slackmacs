use emacs::{defun, Env, Result};

use crate::custom_errors::api_error;

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
