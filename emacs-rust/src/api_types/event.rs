use std::collections::HashMap;

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
    #[serde(skip_serializing_if = "Option::is_none")]
    thread_ts: Option<String>,
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
    pub deleted_ts: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<EventMessageInfo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub previous_message: Option<EventMessageInfo>,
    pub ts: String,
}

impl MessageEvent {
    fn handle_base_event(&self, messages: &mut HashMap<String, BaseMessage>) {
        if self.thread_ts.is_some() {
            let reply = ReplyMessage::from(self.clone());
            let thread_ts = self.thread_ts.as_ref().unwrap();
            let parent_message = match messages.get_mut(thread_ts) {
                Some(message) => message,
                None => return (),
            };
            let replies = match &mut parent_message.replies {
                Some(replies) => replies,
                None => {
                    parent_message.replies = Some(HashMap::new());
                    parent_message.replies.as_mut().unwrap()
                }
            };
            if replies.get(&self.ts).is_some() {
                return ();
            }
            replies.insert(self.ts.to_string(), reply);
            parent_message.reply_count = Some(parent_message.reply_count.unwrap_or(0) + 1);
        } else {
            let message = BaseMessage::from(self.clone());
            messages.insert(self.ts.to_string(), message);
        }
        return ();
    }
    fn handle_message_changed(&self, messages: &mut HashMap<String, BaseMessage>) {
        let event_info = self.message.as_ref().unwrap();
        if event_info.thread_ts.is_some() {
            let thread_ts = event_info.thread_ts.as_ref().unwrap();
            let parent_message = match messages.get_mut(thread_ts) {
                Some(message) => message,
                None => return (),
            };
            let replies = match &mut parent_message.replies {
                Some(replies) => replies,
                None => return (),
            };
            let message = match replies.get_mut(self.ts.as_str()) {
                Some(message) => message,
                None => return (),
            };
            message.text = Some(event_info.text.clone());
        } else {
            let message = match messages.get_mut(self.ts.as_str()) {
                Some(message) => message,
                None => return (),
            };
            message.text = Some(event_info.text.clone());
        }
    }

    fn handle_message_deleted(&self, messages: &mut HashMap<String, BaseMessage>) {
        let deleted_ts = self.deleted_ts.as_ref().unwrap();
        let event_info = self.previous_message.as_ref().unwrap();
        if event_info.thread_ts.is_some() {
            let thread_ts = event_info.thread_ts.as_ref().unwrap();
            let parent_message = match messages.get_mut(thread_ts) {
                Some(message) => message,
                None => return (),
            };
            let replies = match &mut parent_message.replies {
                Some(replies) => replies,
                None => return (),
            };
            if replies.get(deleted_ts).is_none() {
                return ();
            }
            replies.remove(deleted_ts);
            match &mut parent_message.reply_count {
                Some(reply_count) => {
                    if *reply_count > 0 {
                        *reply_count -= 1;
                    }
                }
                None => return (),
            };
        } else {
            messages.remove(deleted_ts);
        }
    }

    fn handle_message_replied(&self, messages: &mut HashMap<String, BaseMessage>) {
        let reply = ReplyMessage::from(self.clone());
        let thread_ts = match &self.thread_ts {
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
        let replies = match &mut parent_message.replies {
            Some(replies) => replies,
            None => {
                parent_message.replies = Some(HashMap::new());
                parent_message.replies.as_mut().unwrap()
            }
        };
        replies.insert(self.ts.to_string(), reply);
        parent_message.reply_count = Some(parent_message.reply_count.unwrap_or(0) + 1);
    }

    pub fn handle_event(&self, slack_instance: &mut Slack) {
        let conversation = match slack_instance.conversations.get_mut(self.channel.as_str()) {
            Some(conversation) => conversation,
            None => return (),
        };

        let messages = match &mut conversation.messages {
            Some(messages) => messages,
            None => return (),
        };

        let message_subtype = match &self.subtype {
            Some(subtype) => subtype,
            None => return self.handle_base_event(messages),
        };
        match MessageSubtype::from_string(message_subtype.to_string()) {
            Some(MessageSubtype::MessageChanged) => return self.handle_message_changed(messages),
            Some(MessageSubtype::MessageDeleted) => return self.handle_message_deleted(messages),
            Some(MessageSubtype::MessageReplied) => return self.handle_message_replied(messages),
            None => Err("Unknown message subtype".to_string()).unwrap(),
        }
    }
}
