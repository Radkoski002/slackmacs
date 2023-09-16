use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Profile {
    #[serde(skip_serializing_if = "Option::is_none")]
    avatar_hash: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    status_text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    status_emoji: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    real_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    display_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    real_name_normalized: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    display_name_normalized: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    email: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    image_24: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    image_32: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    image_48: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    image_72: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    image_192: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    image_512: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    team: Option<String>,
}
