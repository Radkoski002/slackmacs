use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use super::MessageEvent;

type ReplyMap = HashMap<String, ReplyMessage>;
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ReplyMessage {
    #[serde(skip_serializing_if = "Option::is_none")]
    r#type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    user: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    thread_ts: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    ts: Option<String>,
}

impl ReplyMessage {
    pub fn get_text(&self) -> String {
        self.text.clone().unwrap()
    }

    pub fn get_user(&self) -> String {
        self.user.clone().unwrap()
    }

    pub fn get_ts(&self) -> String {
        self.ts.clone().unwrap()
    }
}

impl From<MessageEvent> for ReplyMessage {
    fn from(message_event: MessageEvent) -> Self {
        ReplyMessage {
            r#type: Some(message_event.r#type),
            user: message_event.user,
            text: message_event.text,
            thread_ts: message_event.thread_ts,
            ts: Some(message_event.ts),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BaseMessage {
    #[serde(skip_serializing_if = "Option::is_none")]
    r#type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    channel: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    user: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    ts: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    reply_count: Option<i16>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub replies: Option<ReplyMap>,
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

impl From<MessageEvent> for BaseMessage {
    fn from(message_event: MessageEvent) -> Self {
        BaseMessage {
            r#type: Some(message_event.r#type),
            channel: Some(message_event.channel),
            user: message_event.user,
            text: message_event.text,
            ts: Some(message_event.ts),
            reply_count: Some(0),
            replies: Some(HashMap::new()),
        }
    }
}
