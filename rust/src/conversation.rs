use emacs::{defun, IntoLisp, Result, Value, Vector};

use crate::api_types::messages::BaseMessage;
use crate::helpers::fetch_api::fetch_api;
use crate::helpers::url_builder::ApiPaths;

type MessageVec = Vec<BaseMessage>;

emacs::define_errors! {
    param_error "Invalid parameter" (wrong_type_argument)
}

#[defun(user_ptr)]
fn get(token: String, cookie: String, channel_id: String) -> Result<MessageVec> {
    let channel_param = format!("channel={}", channel_id);
    let client = reqwest::blocking::Client::new();
    let json = fetch_api(
        client,
        &token,
        &cookie,
        ApiPaths::ConversationsHistory,
        Some(channel_param),
    );
    let messages = json.get("messages").unwrap().to_string();
    let parsed_messages: MessageVec = serde_json::from_str(&messages).unwrap();
    Ok(parsed_messages)
}

#[defun]
fn parse<'a>(messages: &MessageVec, params: Vector<'a>) -> Result<Value<'a>> {
    let env = params.value().env;
    let mut parsed_messages = vec![];
    for message in messages {
        let mut parsed_message = vec![];
        for param in params {
            let rust_param = param.into_rust::<String>()?;
            match rust_param.as_str() {
                "text" => parsed_message.push(message.get_text().into_lisp(env)?),
                _ => return env.signal(param_error, ("associated", "DATA", 7)),
            }
        }
        parsed_messages.push(env.call("list", &parsed_message)?);
    }
    Ok(env.call("list", &parsed_messages)?)
}
