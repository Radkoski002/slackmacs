use emacs::{defun, Env, Result};

use crate::{
    api_types::{Slack, User},
    custom_errors::api_error,
    helpers::get_rust_vector_from_json,
};

#[defun]
fn from_json<'a>(json: String, slack_instance: &mut Slack, env: &'a Env) -> Result<()> {
    let json_clone = json.clone();
    let rust_vector = get_rust_vector_from_json::<User>(json_clone, "members".to_string());
    match rust_vector {
        Ok(rust_vector) => {
            for user in rust_vector {
                slack_instance.users.insert(user.get_id(), user);
            }
        }
        Err(error) => env.signal(api_error, (error.message,))?,
    }
    Ok(())
}
