use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
struct ConnectionUser {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct ConnectionTeam {
    #[serde(skip_serializing_if = "Option::is_none")]
    domain: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Connection {
    #[serde(skip_serializing_if = "Option::is_none")]
    ok: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", rename = "self")]
    user: Option<ConnectionUser>,
    #[serde(skip_serializing_if = "Option::is_none")]
    team: Option<ConnectionTeam>,
    #[serde(skip_serializing_if = "Option::is_none")]
    url: Option<String>,
}
