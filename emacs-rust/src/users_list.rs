use emacs::{defun, Env, Result, Value};

use crate::{
    api_types::{Slack, User},
    custom_errors::api_error,
    helpers::{get_rust_vector_from_json, get_vector_from_json},
};

#[defun]
fn from_json<'a>(json: String, slack_instance: &mut Slack, env: &'a Env) -> Result<Value<'a>> {
    let json_clone = json.clone();
    let vector = get_vector_from_json(json, "members".to_string(), env);
    let rust_vector = get_rust_vector_from_json::<User>(json_clone, "members".to_string());
    match rust_vector {
        Ok(rust_vector) => {
            for user in rust_vector {
                slack_instance.users.insert(user.get_id(), user);
            }
        }
        Err(error) => env.signal(api_error, (error.message,))?,
    }
    match vector {
        Ok(vector) => Ok(env.list(&vector)?),
        Err(error) => env.signal(api_error, (error.message,))?,
    }
}
