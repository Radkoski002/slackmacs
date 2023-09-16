use emacs::{defun, IntoLisp, Result, Value, Vector};
use reqwest;

use crate::api_types::user::User;
use crate::helpers::fetch_api::fetch_api;
use crate::helpers::url_builder::ApiPaths;

type UserVec = Vec<User>;

#[defun(user_ptr)]
fn get(token: String, cookie: String) -> Result<Vec<User>> {
    let client = reqwest::blocking::Client::new();
    let json = fetch_api(client, &token, &cookie, ApiPaths::UsersList, None);
    let members = json.get("members").unwrap().to_string();
    let parsed_members: Vec<User> = serde_json::from_str(&members).unwrap();
    Ok(parsed_members)
}

#[defun]
fn parse<'a>(users: &UserVec, params: Vector<'a>) -> Result<Value<'a>> {
    let env = params.value().env;
    let mut parsed_users = vec![];
    for user in users {
        let mut parsed_user = vec![];
        for param in params {
            let rust_param = param.into_rust::<String>()?;
            match rust_param.as_str() {
                "id" => parsed_user.push(user.get_id().into_lisp(env)?),
                "name" => parsed_user.push(user.get_name().into_lisp(env)?),
                "real_name" => parsed_user.push(user.get_real_name().into_lisp(env)?),
                _ => panic!("Unknown parameter"),
            }
        }
        parsed_users.push(env.call("list", &parsed_user)?);
    }
    Ok(env.call("list", &parsed_users)?)
}
