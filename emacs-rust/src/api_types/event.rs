use serde::{Deserialize, Serialize};

pub enum EventType {
    Generic(Event),
    MessageEvent(MessageEvent),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Event {
    pub r#type: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct MessageEvent {
    pub r#type: String,
    pub channel: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
    pub ts: String,
}
