use serde::{Deserialize, Serialize};

use super::{BaseMessage, ReplyMessage, Slack};

pub enum EventType {
    Generic(Event),
    MessageEvent(MessageEvent),
}

enum MessageSubtype {
    MessageChanged,
    MessageDeleted,
    MessageReplied,
}

impl MessageSubtype {
    fn from_string(subtype: String) -> Option<MessageSubtype> {
        match subtype.as_str() {
            "message_changed" => Some(MessageSubtype::MessageChanged),
            "message_deleted" => Some(MessageSubtype::MessageDeleted),
            "message_replied" => Some(MessageSubtype::MessageReplied),
            _ => None,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Event {
    pub r#type: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct EventMessageInfo {
    user: String,
    ts: String,
    text: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct MessageEvent {
    pub r#type: String,
    pub channel: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub subtype: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub thread_ts: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<EventMessageInfo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub previous_message: Option<EventMessageInfo>,
    pub ts: String,
}

impl MessageEvent {
    pub fn handle_event(&self, slack_instance: &mut Slack) {
        let conversation = match slack_instance.conversations.get_mut(self.channel.as_str()) {
            Some(conversation) => conversation,
            None => {
                return ();
            }
        };

        let messages = match conversation.messages.as_mut() {
            Some(messages) => messages,
            None => {
                return ();
            }
        };

        let message_subtype = match self.subtype.as_ref() {
            Some(subtype) => subtype,
            None => {
                let message = BaseMessage::from(self.clone());
                messages.insert(self.ts.to_string(), message);
                return ();
            }
        };
        match MessageSubtype::from_string(message_subtype.to_string()) {
            Some(MessageSubtype::MessageChanged) => {
                let message = match messages.get_mut(self.ts.as_str()) {
                    Some(message) => message,
                    None => {
                        return ();
                    }
                };
                message.text = Some(self.message.as_ref().unwrap().text.clone());
            }
            Some(MessageSubtype::MessageDeleted) => {
                let deleted_ts = self.previous_message.as_ref().unwrap().ts.as_str();
                messages.remove(deleted_ts);
            }
            Some(MessageSubtype::MessageReplied) => {
                let reply = ReplyMessage::from(self.clone());
                let thread_ts = match self.thread_ts.as_ref() {
                    Some(thread_ts) => thread_ts,
                    None => {
                        return ();
                    }
                };
                let parent_message = match messages.get_mut(thread_ts) {
                    Some(message) => message,
                    None => {
                        return ();
                    }
                };
                let replies = match parent_message.replies.as_mut() {
                    Some(replies) => replies,
                    None => {
                        return ();
                    }
                };
                replies.insert(self.ts.to_string(), reply);
            }
            None => Err("Unknown message subtype".to_string()).unwrap(),
        }
    }
}
