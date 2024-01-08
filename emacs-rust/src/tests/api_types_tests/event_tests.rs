use std::collections::HashMap;

use crate::{
    api_types::{BaseMessage, MessageEvent, ReplyMessage, Slack},
    tests::test_utils::{get_mocked_slackmacs_instance, read_mocked_json_file},
    websocket::handle_events,
};

fn get_messages_from_instance(
    slack_instance: &Slack,
) -> &HashMap<std::string::String, BaseMessage> {
    let conversation = slack_instance.conversations.get("C1").unwrap();
    conversation.messages.as_ref().unwrap()
}

fn get_message_from_instance<'a>(
    ts: &'a str,
    slack_instance: &'a Slack,
) -> Option<&'a BaseMessage> {
    let messages = get_messages_from_instance(slack_instance);
    let message = messages.get(ts);
    message.clone()
}

fn get_reply_from_instance<'a>(
    parent_ts: &'a str,
    ts: &'a str,
    slack_instance: &'a Slack,
) -> Option<&'a ReplyMessage> {
    let message = get_message_from_instance(parent_ts, slack_instance);
    let replies = message.unwrap().replies.as_ref().unwrap();
    let reply = replies.get(ts);
    reply.clone()
}

#[test]
fn test_message_send_event() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let instance_clone = slack_instance.clone();
    let send_event = read_mocked_json_file("new_message_event");
    let messages = get_messages_from_instance(&instance_clone);
    assert_eq!(messages.len(), 4);
    let new_message_before = messages.get("8");
    assert!(new_message_before.is_none());
    let _ = handle_events(send_event, &mut slack_instance);
    let messages = get_messages_from_instance(&slack_instance);
    assert_eq!(messages.len(), 5);
    let message = messages.get("8").unwrap();
    assert_eq!(message.get_text(), "new message");
    assert_eq!(message.get_user(), "U1");
    assert_eq!(message.get_ts(), "8");
    assert_eq!(message.get_reply_count(), 0);
}

#[test]
fn test_message_edit_event() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let instance_clone = slack_instance.clone();
    let edit_event = read_mocked_json_file("edit_message_event");
    let messages = get_messages_from_instance(&instance_clone);
    assert_eq!(messages.len(), 4);
    let old_message = messages.get("4").unwrap();
    assert_eq!(old_message.get_text(), "Greetings!");
    assert_eq!(old_message.get_user(), "USLACKBOT");
    assert_eq!(old_message.get_ts(), "4");
    assert_eq!(old_message.get_reply_count(), 0);
    let _ = handle_events(edit_event, &mut slack_instance);
    let messages = get_messages_from_instance(&slack_instance);
    assert_eq!(messages.len(), 4);
    let new_message = messages.get("4").unwrap();
    assert_eq!(new_message.get_text(), "edited message");
    assert_eq!(new_message.get_user(), "USLACKBOT");
    assert_eq!(new_message.get_ts(), "4");
    assert_eq!(new_message.get_reply_count(), 0);
}

#[test]
fn test_message_delete_event() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let instance_clone = slack_instance.clone();
    let delete_event = read_mocked_json_file("delete_message_event");
    let messages = get_messages_from_instance(&instance_clone);
    assert_eq!(messages.len(), 4);
    let old_message = messages.get("4").unwrap();
    assert_eq!(old_message.get_text(), "Greetings!");
    assert_eq!(old_message.get_user(), "USLACKBOT");
    assert_eq!(old_message.get_ts(), "4");
    assert_eq!(old_message.get_reply_count(), 0);
    let _ = handle_events(delete_event, &mut slack_instance);
    let messages = get_messages_from_instance(&slack_instance);
    assert_eq!(messages.len(), 3);
    let new_message = messages.get("4");
    assert!(new_message.is_none());
}

#[test]
fn test_reply_send_event() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let instance_clone = slack_instance.clone();
    let send_event = read_mocked_json_file("new_reply_event");
    let messages = get_messages_from_instance(&instance_clone);
    assert_eq!(messages.len(), 4);
    let new_message_before = messages.get("1").unwrap();
    assert_eq!(new_message_before.get_reply_count(), 2);
    let reply_before = get_reply_from_instance("1", "9", &instance_clone);
    assert!(reply_before.is_none());
    let _ = handle_events(send_event, &mut slack_instance);
    let messages = get_messages_from_instance(&slack_instance);
    assert_eq!(messages.len(), 4);
    let new_message_after = messages.get("1").unwrap();
    assert_eq!(new_message_after.get_reply_count(), 3);
    let reply_after = get_reply_from_instance("1", "9", &slack_instance).unwrap();
    assert_eq!(reply_after.get_text(), "new reply");
    assert_eq!(reply_after.get_user(), "U1");
}

#[test]
fn test_reply_delete_event() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let instance_clone = slack_instance.clone();
    let delete_event = read_mocked_json_file("delete_reply_event");
    let messages = get_messages_from_instance(&instance_clone);
    assert_eq!(messages.len(), 4);
    let old_message = messages.get("1").unwrap();
    assert_eq!(old_message.get_reply_count(), 2);
    let old_reply = get_reply_from_instance("1", "6", &slack_instance);
    assert!(old_reply.is_some());
    let _ = handle_events(delete_event, &mut slack_instance);
    let messages = get_messages_from_instance(&slack_instance);
    assert_eq!(messages.len(), 4);
    let new_message = messages.get("1").unwrap();
    assert_eq!(new_message.get_reply_count(), 1);
    let new_reply = get_reply_from_instance("1", "6", &slack_instance);
    assert!(new_reply.is_none());
}

#[test]
fn test_reply_edit_event() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let instance_clone = slack_instance.clone();
    let edit_event = read_mocked_json_file("edit_reply_event");
    let messages = get_messages_from_instance(&instance_clone);
    assert_eq!(messages.len(), 4);
    let old_message = messages.get("1").unwrap();
    assert_eq!(old_message.get_reply_count(), 2);
    let old_reply = get_reply_from_instance("1", "6", &slack_instance).unwrap();
    assert_eq!(old_reply.get_text(), "how are you?");
    let _ = handle_events(edit_event, &mut slack_instance);
    let messages = get_messages_from_instance(&slack_instance);
    assert_eq!(messages.len(), 4);
    let new_message = messages.get("1").unwrap();
    assert_eq!(new_message.get_reply_count(), 2);
    let new_reply = get_reply_from_instance("1", "6", &slack_instance).unwrap();
    assert_eq!(new_reply.get_text(), "edited reply");
    assert_eq!(new_reply.get_user(), "U2");
}

#[test]
fn test_unknown_event_type() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let event = read_mocked_json_file("new_message_event");
    let mut json: Vec<MessageEvent> = serde_json::from_str(&event.to_string()).unwrap();
    json[0].subtype = Some("unknown".to_string());
    let expected = "Unknown message subtype".to_string();
    assert_eq!(
        json[0]
            .handle_event(&mut slack_instance)
            .unwrap_err()
            .message,
        expected
    );
    let json_string = serde_json::to_string(&json).unwrap();
    let _ = handle_events(json_string, &mut slack_instance);
}

#[test]
fn test_generic_event() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let event = read_mocked_json_file("new_message_event");
    let mut json: Vec<MessageEvent> = serde_json::from_str(&event.to_string()).unwrap();
    json[0].subtype = None;
    json[0].r#type = "unknown".to_string();
    let json_string = serde_json::to_string(&json).unwrap();
    let _ = handle_events(json_string, &mut slack_instance);
}

#[test]
fn test_reply_message_to_new_thread() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let event = read_mocked_json_file("new_reply_event");
    let mut json: Vec<MessageEvent> = serde_json::from_str(&event.to_string()).unwrap();
    json[0].thread_ts = Some("3".to_string());
    let json_string = serde_json::to_string(&json).unwrap();
    let prev_message = get_message_from_instance("3", &slack_instance).unwrap();
    assert!(prev_message.replies.is_none());
    let _ = handle_events(json_string, &mut slack_instance);
    let new_message = get_message_from_instance("3", &slack_instance).unwrap();
    assert_eq!(new_message.get_reply_count(), 1);
}
