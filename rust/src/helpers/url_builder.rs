use std::env;

pub enum ApiPaths {
    UsersList,
    ConverstaionList,
    SendMessage,
}

const fn get_api_path(path: ApiPaths) -> &'static str {
    match path {
        ApiPaths::UsersList => "users.list",
        ApiPaths::ConverstaionList => "conversations.list",
        ApiPaths::SendMessage => "chat.postMessage",
    }
}

pub fn get_url(token: &String, path: ApiPaths, params: Option<String>) -> String {
    let url = format!(
        "https://slack.com/api/{}?token={}&{}",
        get_api_path(path),
        token,
        params.unwrap_or("".to_string())
    );
    url
}
