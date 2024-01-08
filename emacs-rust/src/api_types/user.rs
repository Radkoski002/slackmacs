use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct User {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    real_name: Option<String>,
}

// TODO: Try to get rid of this clones
impl User {
    pub fn get_id(&self) -> String {
        self.id.clone().unwrap()
    }

    pub fn get_name(&self) -> String {
        self.name.clone().unwrap_or(self.get_id())
    }

    pub fn get_real_name(&self) -> String {
        self.real_name.clone().unwrap_or(self.get_name())
    }
}
