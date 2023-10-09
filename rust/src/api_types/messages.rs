use serde::{Deserialize, Serialize};

use crate::custom_errors::ParamError;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BaseMessage {
    #[serde(skip_serializing_if = "Option::is_none")]
    r#type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    channel: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    user: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    ts: Option<String>,
}

impl BaseMessage {
    pub fn get_text(&self) -> String {
        self.text.clone().unwrap()
    }

    pub fn get_user(&self) -> String {
        self.user.clone().unwrap()
    }
}

pub fn message_matcher(message: &BaseMessage, param: String) -> Result<String, ParamError> {
    match param.as_str() {
        "text" => Ok(message.get_text()),
        "user" => Ok(message.get_user()),
        _ => Err(ParamError {
            message: format!("Invalid param: {}", param),
        }),
    }
}
