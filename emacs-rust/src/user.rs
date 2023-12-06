use emacs::{defun, Result};

use crate::api_types::User;

#[defun(user_ptr)]
fn from_json(json: String) -> Result<User> {
    let parsed_json = serde_json::from_str::<User>(&json).unwrap();
    Ok(parsed_json)
}

#[defun]
fn get_id(user: &User) -> Result<String> {
    Ok(user.get_id())
}

#[defun]
fn get_name(user: &User) -> Result<String> {
    Ok(user.get_name())
}
