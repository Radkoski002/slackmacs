use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Conversation {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    user: Option<String>,
}

// TODO: Try to get rid of this clones
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
