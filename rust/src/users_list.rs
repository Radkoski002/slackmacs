use std::thread;

use emacs::{defun, Env, Result, Value, Vector};

use crate::api_helpers::{get_data, parse_data_vec};
use crate::api_types::user::{user_matcher, User};
use crate::custom_errors::api_error;
use crate::helpers::handle_long_operation::handle_long_operation;
use crate::helpers::url_builder::ApiPaths;

type UserVec = Vec<User>;

#[defun(user_ptr)]
fn get(token: String, cookie: String, env: &Env) -> Result<Vec<User>> {
    let (tx, rx) = std::sync::mpsc::channel::<i32>();
    let handle = thread::spawn(move || {
        let data = get_data::<UserVec>(
            cookie,
            token,
            ApiPaths::UsersList,
            "members".to_string(),
            None,
        );
        tx.send(1).unwrap();
        data
    });
    handle_long_operation(env, rx, Some(String::from("Fetching users")))?;
    let users_data = handle.join().unwrap();
    match users_data {
        Ok(data) => return Ok(data),
        Err(err) => return env.signal(api_error, (err.message,))?,
    }
}

#[defun]
fn parse<'a>(users: &UserVec, params: Vector<'a>) -> Result<Value<'a>> {
    parse_data_vec(users, params, user_matcher)
}
