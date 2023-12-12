use serde::{Deserialize, Serialize};

use super::profile::Profile;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct User {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    team_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    deleted: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    color: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    real_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    tz: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    tz_label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    tz_offset: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    profile: Option<Profile>,
    #[serde(skip_serializing_if = "Option::is_none")]
    is_admin: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    is_owner: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    is_primary_owner: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    is_restricted: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    is_ultra_restricted: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    is_bot: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    updated: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    is_app_user: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    has_2fa: Option<bool>,
}

// TODO: Try to get rid of this clones
impl User {
    pub fn get_id(&self) -> String {
        self.id.clone().unwrap()
    }

    pub fn get_name(&self) -> String {
        self.name.clone().unwrap()
    }
}
