use crate::{
    tests::test_utils::{get_mocked_slackmacs_instance, read_mocked_json_file},
    websocket::handle_events,
};

#[test]
fn test_message_send_event() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let instance_clone = slack_instance.clone();
    let send_event = read_mocked_json_file("new_message_event");
    let conversation = instance_clone.conversations.get("C00000001").unwrap();
    let messages = conversation.messages.as_ref().unwrap();
    let new_message_before = messages.get("1703853038.523419");
    assert_eq!(messages.len(), 4);
    assert!(new_message_before.is_none());
    let _ = handle_events(send_event, &mut slack_instance);
    let conversation = slack_instance.conversations.get("C00000001").unwrap();
    let messages = conversation.messages.as_ref().unwrap();
    let new_message_after = messages.get("1703853038.523419");
    assert!(new_message_after.is_some());
    assert_eq!(messages.len(), 5);
    let message = new_message_after.unwrap();
    assert_eq!(message.get_text(), "new message");
    assert_eq!(message.get_user(), "U00000001");
    assert_eq!(message.get_ts(), "1703853038.523419");
    assert_eq!(message.get_reply_count(), 0);
}

#[test]
fn test_message_edit_event() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let instance_clone = slack_instance.clone();
    let edit_event = read_mocked_json_file("edit_message_event");
    let conversation = instance_clone.conversations.get("C00000001").unwrap();
    let messages = conversation.messages.as_ref().unwrap();
    let old_message = messages.get("1703853029.543409");
    assert!(old_message.is_some());
    let old_message = messages.get("1703853029.543409").unwrap();
    assert_eq!(old_message.get_text(), "Greetings!");
    assert_eq!(old_message.get_user(), "USLACKBOT");
    assert_eq!(old_message.get_ts(), "1703853029.543409");
    assert_eq!(old_message.get_reply_count(), 0);
    assert_eq!(messages.len(), 4);
    let _ = handle_events(edit_event, &mut slack_instance);
    let conversation = slack_instance.conversations.get("C00000001").unwrap();
    let messages = conversation.messages.as_ref().unwrap();
    let new_message = messages.get("1703853029.543409");
    assert!(new_message.is_some());
    let new_message = messages.get("1703853029.543409").unwrap();
    assert_eq!(new_message.get_text(), "edited message");
    assert_eq!(new_message.get_user(), "USLACKBOT");
    assert_eq!(new_message.get_ts(), "1703853029.543409");
    assert_eq!(new_message.get_reply_count(), 0);
    assert_eq!(messages.len(), 4);
}

#[test]
fn test_message_delete_event() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let instance_clone = slack_instance.clone();
    let delete_event = read_mocked_json_file("delete_message_event");
    let conversation = instance_clone.conversations.get("C00000001").unwrap();
    let messages = conversation.messages.as_ref().unwrap();
    let old_message = messages.get("1703853029.543409");
    assert!(old_message.is_some());
    let old_message = messages.get("1703853029.543409").unwrap();
    assert_eq!(old_message.get_text(), "Greetings!");
    assert_eq!(old_message.get_user(), "USLACKBOT");
    assert_eq!(old_message.get_ts(), "1703853029.543409");
    assert_eq!(old_message.get_reply_count(), 0);
    assert_eq!(messages.len(), 4);
    let _ = handle_events(delete_event, &mut slack_instance);
    let conversation = slack_instance.conversations.get("C00000001").unwrap();
    let messages = conversation.messages.as_ref().unwrap();
    let new_message = messages.get("1703853029.543409");
    assert!(new_message.is_none());
    assert_eq!(messages.len(), 3);
}

#[test]
fn test_reply_send_event() {
    let mut slack_instance = get_mocked_slackmacs_instance();
    let instance_clone = slack_instance.clone();
    let send_event = read_mocked_json_file("new_reply_event");
    let conversation = instance_clone.conversations.get("C00000001").unwrap();
    let messages = conversation.messages.as_ref().unwrap();
    let new_message_before = messages.get("1698435687.161709");
    assert_eq!(messages.len(), 4);
    assert!(new_message_before.is_some());
    let new_message_before = messages.get("1698435687.161709").unwrap();
    assert_eq!(new_message_before.get_reply_count(), 2);
    let _ = handle_events(send_event, &mut slack_instance);
    let conversation = slack_instance.conversations.get("C00000001").unwrap();
    let messages = conversation.messages.as_ref().unwrap();
    let new_message_after = messages.get("1698435687.161709");
    assert!(new_message_after.is_some());
    assert_eq!(messages.len(), 4);
    let message = new_message_after.unwrap();
    assert_eq!(message.get_reply_count(), 3);
}
