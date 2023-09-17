use serde::{Deserialize, Serialize};

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
}

pub fn message_matcher(message: &BaseMessage, param: String) -> String {
    match param.as_str() {
        "text" => message.get_text(),
        _ => panic!("Invalid parameter"),
    }
}
