use emacs::{defun, Result, Value, Vector};

use crate::api_helpers::{get_data, parse_data_vec};

use crate::api_types::conversation::{conversation_matcher, Conversation};
use crate::helpers::url_builder::ApiPaths;

type ConversationVec = Vec<Conversation>;

emacs::define_errors! {
    param_error "Invalid parameter" (wrong_type_argument)
}

#[defun(user_ptr)]
fn get(token: String, cookie: String) -> Result<ConversationVec> {
    let conversation_types = "types=public_channel,private_channel,mpim,im".to_string();
    let data = get_data::<ConversationVec>(
        cookie,
        token,
        ApiPaths::ConversationList,
        "channels".to_string(),
        Some(conversation_types),
    );
    Ok(data)
}

#[defun]
fn parse<'a>(channels: &ConversationVec, params: Vector<'a>) -> Result<Value<'a>> {
    parse_data_vec(channels, params, conversation_matcher)
}
