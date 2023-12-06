#[allow(dead_code)]
pub enum ApiPaths {
    UsersList,
    ConversationList,
    SendMessage,
    ConversationsHistory,
    Websocket,
}

const fn get_api_path(path: ApiPaths) -> &'static str {
    match path {
        ApiPaths::UsersList => "users.list",
        ApiPaths::ConversationList => "conversations.list",
        ApiPaths::SendMessage => "chat.postMessage",
        ApiPaths::ConversationsHistory => "conversations.history",
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
