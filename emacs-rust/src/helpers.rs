use crate::custom_errors::ParamError;
use emacs::IntoLisp;

fn is_json_ok(json: &serde_json::Value) -> bool {
    let status = json.get("ok").unwrap().to_string();
    status == "true"
}

fn parse_json_from_string(json_string: String) -> Result<serde_json::Value, ParamError> {
    let parsed_json = serde_json::from_str::<serde_json::Value>(&json_string).unwrap();
    if !is_json_ok(&parsed_json) {
        return Err(ParamError {
            message: parsed_json.get("error").unwrap().to_string(),
        });
    }
    Ok(parsed_json)
}

pub fn get_vector_from_json(
    json_string: String,
    json_field: String,
    env: &emacs::Env,
) -> Result<Vec<emacs::Value>, ParamError> {
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
                    final_vec.push(message.to_string().into_lisp(env).unwrap())
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