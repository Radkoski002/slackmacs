use emacs::{defun, Env, Result};

use crate::{
    api_types::{Slack, User},
    custom_errors::{api_error, ParamError},
    helpers::get_rust_vector_from_json,
};

pub fn populate_users(
    json: String,
    slack_instance: &mut Slack,
) -> std::result::Result<(), ParamError> {
    let rust_vector = get_rust_vector_from_json::<User>(json, "members".to_string());
    match rust_vector {
        Ok(rust_vector) => {
            for user in rust_vector {
                slack_instance.users.insert(user.get_id(), user);
            }
        }
        Err(error) => return Err(error),
    }
    Ok(())
}

#[defun]
fn from_json<'a>(json: String, slack_instance: &mut Slack, env: &'a Env) -> Result<()> {
    match populate_users(json, slack_instance) {
        Ok(()) => (),
        Err(error) => env.signal(api_error, (error.message,))?,
    }
    Ok(())
}
