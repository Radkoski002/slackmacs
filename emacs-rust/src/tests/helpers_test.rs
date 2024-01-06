use crate::helpers;

#[test]
fn test_get_value_from_correct_json() {
    let json_string = r#"{"ok": true, "field": "value"}"#.to_string();
    let json_field = "field".to_string();
    let result = helpers::get_value_from_json::<String>(json_string, json_field);
    match result {
        Ok(value) => assert_eq!(value, "value".to_string()),
        Err(_) => assert!(false),
    }
}

#[test]
fn test_get_value_from_json_without_ok() {
    let json_string = r#"{"field": "value"}"#.to_string();
    let json_field = "field".to_string();
    let result = helpers::get_value_from_json::<String>(json_string, json_field);
    match result {
        Ok(_) => assert!(false),
        Err(error) => assert_eq!(error.message, "Ok field doesn't exist".to_string()),
    }
}

#[test]
fn test_get_value_from_incorrect_json_without_error() {
    let json_string = r#"{"ok": false}"#.to_string();
    let json_field = "field".to_string();
    let result = helpers::get_value_from_json::<String>(json_string, json_field);
    match result {
        Ok(_) => assert!(false),
        Err(error) => assert_eq!(error.message, "Error field doesn't exist".to_string()),
    }
}

#[test]
fn test_get_value_from_incorrect_json_with_incorrect_error_type() {
    let json_string = r#"{"ok": false, "error": true}"#.to_string();
    let json_field = "field".to_string();
    let result = helpers::get_value_from_json::<String>(json_string, json_field);
    match result {
        Ok(_) => assert!(false),
        Err(error) => assert_eq!(error.message, "Error field is not a string".to_string()),
    }
}

#[test]
fn test_get_value_from_json_with_invalid_ok_type() {
    let json_string = r#"{"ok": "123", "field": "value"}"#.to_string();
    let json_field = "field".to_string();
    let result = helpers::get_value_from_json::<String>(json_string, json_field);
    match result {
        Ok(_) => assert!(false),
        Err(error) => assert_eq!(error.message, "Ok field is not a boolean".to_string()),
    }
}

#[test]
fn test_get_value_from_json_with_error() {
    let json_string = r#"{"ok": false, "error": "Some kind of api error message"}"#.to_string();
    let json_field = "field".to_string();
    let result = helpers::get_value_from_json::<String>(json_string, json_field);
    match result {
        Ok(_) => assert!(false),
        Err(error) => assert_eq!(error.message, "Some kind of api error message".to_string()),
    }
}

#[test]
fn test_get_value_from_json_with_incorrect_formatting() {
    let json_string = r#"{"ok": true, "field": "value""#.to_string();
    let json_field = "incorrect_field".to_string();
    let result = helpers::get_value_from_json::<String>(json_string, json_field);
    match result {
        Ok(_) => assert!(false),
        Err(_) => assert!(true),
    }
}

#[test]
fn test_get_value_from_json_with_incorrect_field() {
    let json_string = r#"{"ok": true, "field": "value"}"#.to_string();
    let json_field = "incorrect_field".to_string();
    let result = helpers::get_value_from_json::<String>(json_string, json_field);
    match result {
        Ok(_) => assert!(false),
        Err(error) => assert_eq!(error.message, "This field doesn't exits".to_string()),
    }
}

#[test]
fn test_get_rust_vector_from_correct_json() {
    let json_string =
        r#"{"ok": true, "field": [{"field1": "value1"}, {"field2": "value2"}]}"#.to_string();
    let json_field = "field".to_string();
    let result = helpers::get_rust_vector_from_json::<serde_json::Value>(json_string, json_field);
    match result {
        Ok(value) => {
            assert_eq!(value[0]["field1"], "value1".to_string());
            assert_eq!(value[1]["field2"], "value2".to_string());
        }
        Err(_) => assert!(false),
    }
}

#[test]
fn test_get_rust_vector_from_incorrect_json() {
    let json_string = r#"{"ok": false, "error": "Some kind of api error message"}"#.to_string();
    let json_field = "field".to_string();
    let result = helpers::get_rust_vector_from_json::<serde_json::Value>(json_string, json_field);
    match result {
        Ok(_) => assert!(false),
        Err(error) => assert_eq!(error.message, "Some kind of api error message".to_string()),
    }
}

#[test]
fn test_get_rust_vector_from_json_with_incorrect_field_name() {
    let json_string =
        r#"{"ok": true, "field": [{"field1": "value1"}, {"field2": "value2"}]}"#.to_string();
    let json_field = "incorrect_field".to_string();
    let result = helpers::get_rust_vector_from_json::<serde_json::Value>(json_string, json_field);
    match result {
        Ok(_) => assert!(false),
        Err(error) => assert_eq!(error.message, "This field doesn't exits".to_string()),
    }
}

#[test]
fn test_get_rust_vector_from_json_with_incorrect_field_type() {
    let json_string = r#"{"ok": true, "field": "value"}"#.to_string();
    let json_field = "field".to_string();
    let result = helpers::get_rust_vector_from_json::<serde_json::Value>(json_string, json_field);
    match result {
        Ok(_) => assert!(false),
        Err(error) => assert_eq!(
            error.message,
            "Value of this field is not an array".to_string()
        ),
    }
}
