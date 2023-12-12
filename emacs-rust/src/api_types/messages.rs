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
    #[serde(skip_serializing_if = "Option::is_none")]
    reply_count: Option<i16>,
}

// TODO: Try to get rid of this clones
impl BaseMessage {
    pub fn get_text(&self) -> String {
        self.text.clone().unwrap()
    }

    pub fn get_user(&self) -> String {
        self.user.clone().unwrap()
    }

    pub fn get_ts(&self) -> String {
        self.ts.clone().unwrap()
    }

    pub fn get_reply_count(&self) -> i16 {
        match self.reply_count.clone() {
            Some(count) => count,
            None => 0,
        }
    }
}
