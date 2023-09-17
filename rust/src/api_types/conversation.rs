use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Conversation {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
}

impl Conversation {
    pub fn get_id(&self) -> String {
        self.id.clone().unwrap()
    }
    pub fn get_name(&self) -> String {
        self.name.clone().unwrap()
    }
}

pub fn conversation_matcher(conversation: &Conversation, param: String) -> String {
    match param.as_str() {
        "id" => conversation.get_id(),
        "name" => conversation.get_name(),
        _ => panic!("Invalid parameter"),
    }
}
