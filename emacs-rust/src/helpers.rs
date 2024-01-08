use crate::custom_errors::ParamError;

fn is_json_ok(json: &serde_json::Value) -> Result<bool, ParamError> {
    let status = match json.get("ok") {
        Some(value) => match value {
            serde_json::Value::Bool(status) => *status,
            _ => {
                return Err(ParamError {
                    message: "Ok field is not a boolean".to_string(),
                })
            }
        },
        None => {
            return Err(ParamError {
                message: "Ok field doesn't exist".to_string(),
            })
        }
    };
    Ok(status)
}

pub fn parse_json_from_string(json_string: String) -> Result<serde_json::Value, ParamError> {
    let parsed_json = match serde_json::from_str::<serde_json::Value>(&json_string) {
        Ok(parsed_json) => parsed_json,
        Err(error) => {
            return Err(ParamError {
                message: error.to_string(),
            });
        }
    };
    let ok_status = match is_json_ok(&parsed_json) {
        Ok(ok_status) => ok_status,
        Err(error) => return Err(error),
    };
    if !ok_status {
        let error_message = match parsed_json.get("error") {
            Some(value) => value,
            None => {
                return Err(ParamError {
                    message: "Error field doesn't exist".to_string(),
                });
            }
        };
        let error_message = match error_message {
            serde_json::Value::String(error_message) => error_message,
            _ => {
                return Err(ParamError {
                    message: "Error field is not a string".to_string(),
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
            return Err(ParamError {
                message: "This field doesn't exits".to_string(),
            });
        }
    };
    match serde_json::from_value::<T>(correct_json.clone()) {
        Ok(parsed_value) => Ok(parsed_value),
        Err(error) => Err(ParamError {
            message: error.to_string(),
        }),
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
        Some(field_value) => match field_value {
            serde_json::Value::Array(vector) => {
                let mut final_vec = vec![];
                for elements in vector {
                    let parsed_value = match serde_json::from_value::<T>(elements.clone()) {
                        Ok(parsed_value) => parsed_value,
                        Err(error) => {
                            return Err(ParamError {
                                message: error.to_string(),
                            });
                        }
                    };
                    final_vec.push(parsed_value)
                }
                return Ok(final_vec);
            }
            _ => {
                return Err(ParamError {
                    message: "Value of this field is not an array".to_string(),
                });
            }
        },
        None => {
            return Err(ParamError {
                message: "This field doesn't exits".to_string(),
            });
        }
    }
}
