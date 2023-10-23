use emacs::{IntoLisp, Value, Vector};

use crate::custom_errors::{param_error, ApiError, ParamError};
use crate::helpers::{fetch_api::fetch_api, url_builder::ApiPaths};

pub fn get_data<T>(
    cookie: String,
    token: String,
    api_path: ApiPaths,
    json_field: String,
    params: Option<String>,
) -> std::result::Result<T, ApiError>
where
    T: serde::de::DeserializeOwned + Send,
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
    let status = json.get("ok").unwrap().to_string();
    if status != "true" {
        let error_message = json.get("error").unwrap().to_string();
        return Err(ApiError {
            message: error_message,
        });
    }
    let results = json.get(json_field).unwrap().to_string();
    let parsed_results: T = serde_json::from_str(&results).unwrap();
    Ok(parsed_results)
}

pub fn parse_data_vec<'a, T>(
    vector: &Vec<T>,
    params: Vector<'a>,
    matcher_func: fn(&T, String) -> std::result::Result<String, ParamError>,
) -> emacs::Result<Value<'a>> {
    let env = params.value().env;
    let mut parsed_elements = vec![];
    for element in vector {
        let mut parsed_element = vec![];
        for param in params {
            let rust_param = param.into_rust::<String>()?;
            match matcher_func(element, rust_param.clone()) {
                Ok(value) => parsed_element.push(value.into_lisp(env)?),
                Err(err) => return env.signal(param_error, (err.message,))?,
            }
        }
        parsed_elements.push(env.list(&parsed_element)?);
    }
    Ok(env.list(&parsed_elements)?)
}
