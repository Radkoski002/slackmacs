use emacs::{defun, Env, Result, Value};

use crate::{
    api_types::{Conversation, Slack},
    custom_errors::api_error,
    helpers::get_rust_vector_from_json,
};

#[defun]
fn from_json<'a>(json: String, slack_instance: &mut Slack, env: &'a Env) -> Result<()> {
    let json_clone = json.clone();
    let conversation_vec =
        get_rust_vector_from_json::<Conversation>(json_clone, "channels".to_string());
    match conversation_vec {
        Ok(rust_vector) => {
            for channel in rust_vector {
                slack_instance
                    .conversations
                    .insert(channel.get_id(), channel);
            }
        }
        Err(error) => env.signal(api_error, (error.message,))?,
    }
    Ok(())
}

#[defun]
fn create_buttons(slack_instance: &Slack, create_callback: Value) -> Result<()> {
    let conversations = slack_instance.conversations.values();
    for conversation in conversations {
        let name = match slack_instance.users.get(conversation.get_user().as_str()) {
            Some(user) => user.get_name(),
            None => conversation.get_name(),
        };
        let id = conversation.get_id();
        create_callback.call((name, id))?;
    }
    Ok(())
}
