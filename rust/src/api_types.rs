use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Profile {
    avatar_hash: String,
    status_text: String,
    status_emoji: String,
    real_name: String,
    display_name: String,
    real_name_normalized: String,
    display_name_normalized: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    email: Option<String>,
    image_24: String,
    image_32: String,
    image_48: String,
    image_72: String,
    image_192: String,
    image_512: String,
    team: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct User {
    id: String,
    team_id: String,
    name: String,
    deleted: bool,
    color: String,
    pub real_name: String,
    tz: String,
    tz_label: String,
    tz_offset: i32,
    profile: Profile,
    is_admin: bool,
    is_owner: bool,
    is_primary_owner: bool,
    is_restricted: bool,
    is_ultra_restricted: bool,
    is_bot: bool,
    updated: i32,
    is_app_user: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    has_2fa: Option<bool>,
}
