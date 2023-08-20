use emacs::{defun, Env, IntoLisp, Result, Value, Vector};
use reqwest;

mod api_types;
mod helpers;
mod test;

use helpers::url_builder::{get_url, ApiPaths};

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

//Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module(name = "slackmacs-module-rs", defun_prefix = "slackmacs")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

#[defun]
fn say_hello(env: &Env, mut name: String) -> Result<Value<'_>> {
    name = name.replace("\n", " ");
    env.message(&format!("{} {}!", test::return_string(), name))
}

fn fetch_api(
    client: reqwest::blocking::Client,
    token: &String,
    cookie: &String,
    path: ApiPaths,
) -> serde_json::Value {
    let res = client
        .post(&get_url(&token, path, None))
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

#[defun(user_ptr)]
fn to_rust_vec_string(input: Vector) -> Result<Vec<String>> {
    let mut vec = vec![];
    for e in input {
        vec.push(e.into_rust()?);
    }
    Ok(vec)
}

#[defun]
fn get_users_list(token: String, cookie: String, env: &Env) -> Result<Value> {
    let client = reqwest::blocking::Client::new();
    let json = fetch_api(client, &token, &cookie, ApiPaths::UsersList);
    let members = json.get("members").unwrap().to_string();
    let parsed_members: Vec<api_types::User> = serde_json::from_str(&members).unwrap();
    let mut usernames = vec![];
    for user in parsed_members {
        usernames.push(user.real_name.into_lisp(env)?);
    }
    let result = env.vector(&usernames)?;
    Ok(result)
}
