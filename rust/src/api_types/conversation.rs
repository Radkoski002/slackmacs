use serde::{Deserialize, Serialize};

use crate::custom_errors::ParamError;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Conversation {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    user: Option<String>,
}

impl Conversation {
    pub fn get_id(&self) -> String {
        self.id.clone().unwrap()
    }
    pub fn get_name(&self) -> String {
        if self.name.is_none() {
            return "".to_string();
        }
        self.name.clone().unwrap()
    }
    pub fn get_user(&self) -> String {
        if self.user.is_none() {
            return "no user".to_string();
        }
        self.user.clone().unwrap()
    }
}

pub fn conversation_matcher(
    conversation: &Conversation,
    param: String,
) -> Result<String, ParamError> {
    match param.as_str() {
        "id" => Ok(conversation.get_id()),
        "name" => Ok(conversation.get_name()),
        "user" => Ok(conversation.get_user()),
        _ => Err(ParamError {
            message: format!("Invalid param: {}", param),
        }),
    }
}
