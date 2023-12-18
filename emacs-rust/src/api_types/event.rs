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
            None => {
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
                    let reply_count = match &mut parent_message.reply_count {
                        Some(reply_count) => reply_count,
                        None => {
                            parent_message.reply_count = Some(0);
                            parent_message.reply_count.as_mut().unwrap()
                        }
                    };
                    replies.insert(self.ts.to_string(), reply);
                    *reply_count += 1;
                } else {
                    let message = BaseMessage::from(self.clone());
                    messages.insert(self.ts.to_string(), message);
                }
                return ();
            }
        };
        match MessageSubtype::from_string(message_subtype.to_string()) {
            Some(MessageSubtype::MessageChanged) => {
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
            Some(MessageSubtype::MessageDeleted) => {
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
            Some(MessageSubtype::MessageReplied) => {
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
                match &mut parent_message.reply_count {
                    Some(reply_count) => *reply_count += 1,
                    None => parent_message.reply_count = Some(1),
                };
            }
            None => Err("Unknown message subtype".to_string()).unwrap(),
        }
    }
}
