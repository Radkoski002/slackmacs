use emacs::{defun, IntoLisp, Result, Value, Vector};
use reqwest;

use crate::api_types::conversation::Conversation;
use crate::helpers::fetch_api::fetch_api;
use crate::helpers::url_builder::ApiPaths;

type ConversationVec = Vec<Conversation>;

emacs::define_errors! {
    param_error "Invalid parameter" (wrong_type_argument)
}

#[defun(user_ptr)]
fn get(token: String, cookie: String) -> Result<ConversationVec> {
    let conversation_types = "types=public_channel,private_channel,mpim,im".to_string();
    let client = reqwest::blocking::Client::new();
    let json = fetch_api(
        client,
        &token,
        &cookie,
        ApiPaths::ConversationList,
        Some(conversation_types),
    );
    let channels = json.get("channels").unwrap().to_string();
    let parsed_channels: Vec<Conversation> = serde_json::from_str(&channels).unwrap();
    Ok(parsed_channels)
}

#[defun]
fn parse<'a>(channels: &ConversationVec, params: Vector<'a>) -> Result<Value<'a>> {
    let env = params.value().env;
    let mut parsed_conversations = vec![];
    for channel in channels {
        let mut parsed_conversation = vec![];
        for param in params {
            let rust_param = param.into_rust::<String>()?;
            match rust_param.as_str() {
                "id" => parsed_conversation.push(channel.get_id().into_lisp(env)?),
                "name" => parsed_conversation.push(channel.get_name().into_lisp(env)?),
                _ => return env.signal(param_error, ("associated", "DATA", 7)),
            }
        }
        parsed_conversations.push(env.call("list", &parsed_conversation)?);
    }
    Ok(env.call("list", &parsed_conversations)?)
}
