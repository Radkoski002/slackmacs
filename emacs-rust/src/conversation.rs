use emacs::{defun, Env, Result};

use crate::custom_errors::buffer_error;

#[defun]
fn check_buffer_name(name: String, env: &Env) -> Result<bool> {
    if !["conversation".to_string(), "reply".to_string()].contains(&name) {
        return env.signal(buffer_error, ("Not a conversation buffer".to_string(),));
    }
    Ok(true)
}

#[defun]
fn get_id_from_buffer_name(buffer_name: String, env: &Env) -> Result<String> {
    let name_split: Vec<&str> = buffer_name.split("-").collect();
    let buffer_name = match name_split.get(0) {
        Some(buffer_name) => buffer_name,
        None => return env.signal(buffer_error, ("Not a conversation buffer".to_string(),)),
    };
    if !["conversation", "reply"].contains(buffer_name) {
        return env.signal(buffer_error, ("Not a conversation buffer".to_string(),));
    }
    let id = match name_split.get(1) {
        Some(id) => id.to_string(),
        None => return env.signal(buffer_error, ("Not a conversation buffer".to_string(),)),
    };
    Ok(id)
}

#[defun]
fn get_ts_from_buffer_name(buffer_name: String, env: &Env) -> Result<String> {
    let name_split: Vec<&str> = buffer_name.split("-").collect();
    let buffer_name = match name_split.get(0) {
        Some(buffer_name) => buffer_name,
        None => return env.signal(buffer_error, ("Not a reply buffer".to_string(),)),
    };
    if name_split.len() != 3 || !["reply"].contains(buffer_name) {
        return env.signal(buffer_error, ("Not a reply buffer".to_string(),));
    }
    let ts = match name_split.get(2) {
        Some(ts) => ts.to_string(),
        None => return env.signal(buffer_error, ("Not a reply buffer".to_string(),)),
    };
    Ok(ts)
}
