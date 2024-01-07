use emacs::{defun, Env, Result, Value};

use crate::{
    api_types::{Conversation, Slack},
    custom_errors::{api_error, ParamError},
    helpers::get_rust_vector_from_json,
};

pub fn populate_conversations(
    json: String,
    slack_instance: &mut Slack,
) -> std::result::Result<(), ParamError> {
    let conversation_vec = get_rust_vector_from_json::<Conversation>(json, "channels".to_string());
    let conversations = match conversation_vec {
        Ok(conversation_vector) => conversation_vector,
        Err(error) => return Err(error),
    };
    for conversation in conversations {
        slack_instance
            .conversations
            .insert(conversation.get_id(), conversation);
    }
    Ok(())
}

#[defun]
fn from_json<'a>(json: String, slack_instance: &mut Slack, env: &'a Env) -> Result<()> {
    match populate_conversations(json, slack_instance) {
        Ok(()) => (),
        Err(error) => env.signal(api_error, (error.message,))?,
    }
    Ok(())
}

fn get_grouped_channels(
    slack_instance: &Slack,
) -> (
    Vec<(String, String)>,
    Vec<(String, String)>,
    Vec<(String, String)>,
) {
    let mut users = vec![];
    let mut multiple_users = vec![];
    let mut channels = vec![];
    let conversations = slack_instance.conversations.values();
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
    channels.sort_by_key(|(name, _id)| name.to_string());
    multiple_users.sort_by_key(|(name, _id)| name.to_string());
    users.sort_by_key(|(name, _id)| name.to_string());
    (users, multiple_users, channels)
}

#[defun]
fn create_buttons(
    slack_instance: &Slack,
    create_callback: Value,
    header_callback: Value,
) -> Result<()> {
    let (users, multiple_users, channels) = get_grouped_channels(slack_instance);
    if channels.len() != 0 {
        header_callback.call(("Channels:".to_string(),))?;
        for (name, id) in channels {
            create_callback.call((name, id))?;
        }
        header_callback.call(("".to_string(),))?;
    }
    if multiple_users.len() != 0 {
        header_callback.call(("Multiple users conversations:".to_string(),))?;
        for (name, id) in multiple_users {
            let formatted_name = name.replace("--", ", ");
            let final_name = formatted_name.split("-").collect::<Vec<&str>>()[1];
            create_callback.call((final_name, id))?;
        }
        header_callback.call(("".to_string(),))?;
    }
    if users.len() != 0 {
        header_callback.call(("Users:".to_string(),))?;
        for (name, id) in users {
            create_callback.call((name, id))?;
        }
    }
    Ok(())
}
