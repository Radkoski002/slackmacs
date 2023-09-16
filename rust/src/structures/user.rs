use emacs::{defun, Result};

use super::api_types::User;

#[defun]
fn get_user_name(user: &User) -> Result<String> {
    Ok(user.get_name())
}

#[defun]
fn get_user_real_name(user: &User) -> Result<String> {
    Ok(user.get_real_name())
}

#[defun]
fn get_user_id(user: &User) -> Result<String> {
    Ok(user.get_id())
}
