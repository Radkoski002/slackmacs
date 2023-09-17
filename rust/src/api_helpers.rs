use emacs::{IntoLisp, Result, Value, Vector};

use crate::helpers::{fetch_api::fetch_api, url_builder::ApiPaths};

pub fn get_data<T>(
    cookie: String,
    token: String,
    api_path: ApiPaths,
    json_field: String,
    params: Option<String>,
) -> T
where
    T: serde::de::DeserializeOwned,
{
    let client = reqwest::blocking::Client::new();
    let json = fetch_api(
        client,
        &token,
        &cookie,
        api_path,
        match params {
            Some(param) => Some(param),
            None => None,
        },
    );
    let messages = json.get(json_field).unwrap().to_string();
    let parsed_messages: T = serde_json::from_str(&messages).unwrap();
    parsed_messages
}

pub fn parse_data_vec<'a, T>(
    vector: &Vec<T>,
    params: Vector<'a>,
    matcher_func: fn(&T, String) -> String,
) -> Result<Value<'a>> {
    let env = params.value().env;
    let mut parsed_elements = vec![];
    for element in vector {
        let mut parsed_element = vec![];
        for param in params {
            let rust_param = param.into_rust::<String>()?;
            parsed_element.push(matcher_func(element, rust_param).into_lisp(env)?);
        }
        parsed_elements.push(env.list(&parsed_element)?);
    }
    Ok(env.list(&parsed_elements)?)
}

