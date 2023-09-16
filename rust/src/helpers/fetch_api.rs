use super::url_builder::{get_url, ApiPaths};

pub fn fetch_api(
    client: reqwest::blocking::Client,
    token: &String,
    cookie: &String,
    path: ApiPaths,
    options: Option<String>,
) -> serde_json::Value {
    let res = client
        .post(&get_url(&token, path, options))
        .header("Cookie", format!("d={}", cookie))
        .send()
        .unwrap();

    let json = res.json::<serde_json::Value>().unwrap();
    match json.get("error") {
        Some(e) => println!("Error: {}", e),
        None => println!("No error"),
    }
    json
}
