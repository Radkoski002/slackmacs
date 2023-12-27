use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use super::BaseMessage;

type MessageMap = HashMap<String, BaseMessage>;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Conversation {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub messages: Option<MessageMap>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_channel: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_group: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_im: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_mpim: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_member: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_user_deleted: Option<bool>,
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
