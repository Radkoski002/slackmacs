use serde::{Deserialize, Serialize};

use crate::custom_errors::ParamError;

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

impl User {
    pub fn get_id(&self) -> String {
        self.id.clone().unwrap()
    }

    pub fn get_name(&self) -> String {
        self.name.clone().unwrap()
    }

    pub fn get_real_name(&self) -> String {
        self.real_name.clone().unwrap()
    }
}

pub fn user_matcher(user: &User, param: String) -> Result<String, ParamError> {
    match param.as_str() {
        "id" => Ok(user.get_id()),
        "name" => Ok(user.get_name()),
        "real_name" => Ok(user.get_real_name()),
        _ => Err(ParamError {
            message: format!("Invalid param: {}", param),
        }),
    }
}
