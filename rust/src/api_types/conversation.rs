use serde::{Deserialize, Serialize};

use crate::custom_errors::ParamError;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Conversation {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
}

impl Conversation {
    pub fn get_id(&self) -> String {
        self.id.clone().unwrap()
    }
    pub fn get_name(&self) -> String {
        self.name.clone().unwrap()
    }
}

pub fn conversation_matcher(
    conversation: &Conversation,
    param: String,
) -> Result<String, ParamError> {
    match param.as_str() {
        "id" => Ok(conversation.get_id()),
        "name" => Ok(conversation.get_name()),
        _ => Err(ParamError {
            message: format!("Invalid param: {}", param),
        }),
    }
}
