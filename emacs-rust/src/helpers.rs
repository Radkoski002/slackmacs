use crate::custom_errors::ParamError;

fn is_json_ok(json: &serde_json::Value) -> bool {
    let status = json.get("ok").unwrap().as_bool().unwrap();
    status
}

fn parse_json_from_string(json_string: String) -> Result<serde_json::Value, ParamError> {
    let parsed_json = serde_json::from_str::<serde_json::Value>(&json_string).unwrap();
    if !is_json_ok(&parsed_json) {
        let error_message = parsed_json.get("error").unwrap();
        return Err(ParamError {
            message: serde_json::from_value::<String>(error_message.clone()).unwrap(),
        });
    }
    Ok(parsed_json)
}

pub fn get_value_from_json<T>(json_string: String, json_field: String) -> Result<T, ParamError>
where
    T: serde::de::DeserializeOwned,
{
    let parsed_json = parse_json_from_string(json_string);
    let correct_json = match parsed_json {
        Ok(parsed_json) => parsed_json,
        Err(error) => return Err(error),
    };
    let correct_json = match correct_json.get(json_field) {
        Some(value) => value,
        None => {
            let error_message = "This field doesn't exits".to_string();
            return Err(ParamError {
                message: error_message,
            });
        }
    };
    let parsed_value = serde_json::from_value::<T>(correct_json.clone()).unwrap();
    Ok(parsed_value)
}

pub fn get_rust_vector_from_json<T>(
    json_string: String,
    // TODO: change this String to some kind of enum
    json_field: String,
) -> Result<Vec<T>, ParamError>
where
    T: serde::de::DeserializeOwned,
{
    let parsed_json = parse_json_from_string(json_string);
    let correct_json = match parsed_json {
        Ok(parsed_json) => parsed_json,
        Err(error) => return Err(error),
    };
    match correct_json.get(json_field) {
        Some(messages) => match messages {
            serde_json::Value::Array(messages) => {
                let mut final_vec = vec![];
                for message in messages {
                    let parsed_value = serde_json::from_value::<T>(message.clone()).unwrap();
                    final_vec.push(parsed_value)
                }
                return Ok(final_vec);
            }
            _ => {
                let error_message = "Value of this field is not an array".to_string();
                return Err(ParamError {
                    message: error_message,
                });
            }
        },
        None => {
            let error_message = "This field doesn't exits".to_string();
            return Err(ParamError {
                message: error_message,
            });
        }
    }
}
