#[allow(dead_code)]
pub enum ApiPaths {
    ConversationHistory,
    ConversationList,
    DeleteMessage,
    SendMessage,
    UsersList,
    Websocket,
}

const fn get_api_path(path: ApiPaths) -> &'static str {
    match path {
        ApiPaths::ConversationHistory => "conversations.history",
        ApiPaths::ConversationList => "conversations.list",
        ApiPaths::DeleteMessage => "chat.delete",
        ApiPaths::SendMessage => "chat.postMessage",
        ApiPaths::UsersList => "users.list",
        ApiPaths::Websocket => "rtm.connect",
    }
}

pub fn get_url(token: &String, path: ApiPaths, params: Option<String>) -> String {
    format!(
        "https://slack.com/api/{}?token={}&{}",
        get_api_path(path),
        token,
        params.unwrap_or("".to_string())
    )
}
