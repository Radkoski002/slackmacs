use emacs::{defun, Env, IntoLisp, Result, Value, Vector};
use reqwest;

mod helpers;
mod structures;
mod test;

use helpers::url_builder::{get_url, ApiPaths};
use structures::api_types::User;

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
fn get_users_list(token: String, cookie: String) -> Result<Vec<User>> {
    let client = reqwest::blocking::Client::new();
    let json = fetch_api(client, &token, &cookie, ApiPaths::UsersList);
    let members = json.get("members").unwrap().to_string();
    let parsed_members: Vec<User> = serde_json::from_str(&members).unwrap();
    Ok(parsed_members)
}

type UserVec = Vec<User>;

#[defun]
fn parse_users_list<'a>(users: &UserVec, params: Vector<'a>) -> Result<Value<'a>> {
    let env = params.value().env;
    let mut test = vec![];
    for param in params {
        test.push(param)
    }
    let mut parsed_users = vec![];
    for user in users {
        let mut parsed_user = vec![];
        for param in params {
            let rust_param = param.into_rust::<String>()?;
            match rust_param.as_str() {
                "id" => parsed_user.push(user.get_id().into_lisp(env)?),
                "name" => parsed_user.push(user.get_name().into_lisp(env)?),
                "real_name" => parsed_user.push(user.get_real_name().into_lisp(env)?),
                _ => panic!("Unknown parameter"),
            }
        }
        parsed_users.push(env.call("list", &parsed_user)?);
    }
    Ok(env.call("list", &parsed_users)?)
}
