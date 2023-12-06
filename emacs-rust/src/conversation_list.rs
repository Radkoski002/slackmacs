use emacs::{defun, Env, Result, Value};

use crate::{custom_errors::api_error, helpers::get_vector_from_json};

#[defun]
fn from_json(json: String, env: &Env) -> Result<Value> {
    let vector = get_vector_from_json(json, "channels".to_string(), env);
    match vector {
        Ok(vector) => Ok(env.list(&vector)?),
        Err(error) => env.signal(api_error, (error.message,))?,
    }
}
