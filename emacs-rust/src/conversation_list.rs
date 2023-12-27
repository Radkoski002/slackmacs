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
fn create_buttons(
    slack_instance: &Slack,
    create_callback: Value,
    header_callback: Value,
) -> Result<()> {
    let conversations = slack_instance.conversations.values();
    let mut users = vec![];
    let mut multiple_users = vec![];
    let mut channels = vec![];
    for conversation in conversations {
        match conversation.is_member {
            Some(false) => continue,
            _ => (),
        }
        match conversation.is_user_deleted {
            Some(true) => continue,
            _ => (),
        }
        match slack_instance.users.get(conversation.get_user().as_str()) {
            Some(user) => users.push((user.get_real_name(), conversation.get_id())),
            None => match conversation.is_mpim {
                Some(true) => multiple_users.push((conversation.get_name(), conversation.get_id())),
                _ => channels.push((conversation.get_name(), conversation.get_id())),
            },
        };
    }
    if channels.len() != 0 {
        channels.sort_by_key(|(name, _id)| name.to_string());
        header_callback.call(("Channels:".to_string(),))?;
        for (name, id) in channels {
            create_callback.call((name, id))?;
        }
        header_callback.call(("".to_string(),))?;
    }
    if multiple_users.len() != 0 {
        multiple_users.sort_by_key(|(name, _id)| name.to_string());
        header_callback.call(("Multiple users conversations:".to_string(),))?;
        for (name, id) in multiple_users {
            let formatted_name = name.replace("--", ", ");
            let final_name = formatted_name.split("-").collect::<Vec<&str>>()[1];
            create_callback.call((final_name, id))?;
        }
        header_callback.call(("".to_string(),))?;
    }
    if users.len() != 0 {
        users.sort_by_key(|(name, _id)| name.to_string());
        header_callback.call(("Users:".to_string(),))?;
        for (name, id) in users {
            create_callback.call((name, id))?;
        }
        header_callback.call(("".to_string(),))?;
    }
    Ok(())
}
