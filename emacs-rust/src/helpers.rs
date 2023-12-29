use crate::custom_errors::ParamError;

fn is_json_ok(json: &serde_json::Value) -> bool {
    let status = match json.get("ok") {
        Some(value) => match value {
            serde_json::Value::Bool(status) => *status,
            _ => return false,
        },
        None => return false,
    };
    status
}

fn parse_json_from_string(json_string: String) -> Result<serde_json::Value, ParamError> {
    let parsed_json = match serde_json::from_str::<serde_json::Value>(&json_string) {
        Ok(parsed_json) => parsed_json,
        Err(error) => {
            let error_message = error.to_string();
            return Err(ParamError {
                message: error_message,
            });
        }
    };
    if !is_json_ok(&parsed_json) {
        let error_message = match parsed_json.get("error") {
            Some(value) => value,
            None => {
                let error_message = "Error field doesn't exist".to_string();
                return Err(ParamError {
                    message: error_message,
                });
            }
        };
        let error_message = match error_message {
            serde_json::Value::String(error_message) => error_message,
            _ => {
                let error_message = "Error field is not a string".to_string();
                return Err(ParamError {
                    message: error_message,
                });
            }
        };
        return Err(ParamError {
            message: error_message.clone(),
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
    match serde_json::from_value::<T>(correct_json.clone()) {
        Ok(parsed_value) => Ok(parsed_value),
        Err(error) => {
            let error_message = error.to_string();
            Err(ParamError {
                message: error_message,
            })
        }
    }
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
                    let parsed_value = match serde_json::from_value::<T>(message.clone()) {
                        Ok(parsed_value) => parsed_value,
                        Err(error) => {
                            let error_message = error.to_string();
                            return Err(ParamError {
                                message: error_message,
                            });
                        }
                    };
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
