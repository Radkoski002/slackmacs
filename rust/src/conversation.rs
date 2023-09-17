use emacs::{defun, Result, Value, Vector};

use crate::api_helpers::{get_data, parse_data_vec};
use crate::api_types::messages::{message_matcher, BaseMessage};
use crate::helpers::url_builder::ApiPaths;

type MessageVec = Vec<BaseMessage>;

emacs::define_errors! {
    param_error "Invalid parameter" (wrong_type_argument)
}

#[defun(user_ptr)]
fn get(token: String, cookie: String, channel_id: String) -> Result<MessageVec> {
    let channel_param = format!("channel={}", channel_id);
    let data = get_data::<MessageVec>(
        cookie,
        token,
        ApiPaths::ConversationsHistory,
        "messages".to_string(),
        Some(channel_param),
    );
    Ok(data)
}

#[defun]
fn parse<'a>(messages: &MessageVec, params: Vector<'a>) -> Result<Value<'a>> {
    parse_data_vec(messages, params, message_matcher)
}
