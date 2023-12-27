use std::collections::HashMap;

use crate::api_types::{BaseMessage, Conversation, MessageEvent, Slack};

const TEST_CHANNEL_ID: &str = "C123456789";
const TEST_MESSAGE_TS: &str = "1234567890.123456";
const TEST_MESSAGE_TEXT: &str = "test message";

fn get_mocked_slack_instance() -> Slack {
    let mocked_channel = Conversation {
        id: Some(TEST_CHANNEL_ID.to_string()),
        name: Some("test_channel".to_string()),
        messages: Some(HashMap::new()),
        user: None,
        is_channel: None,
        is_group: None,
        is_im: None,
        is_mpim: None,
        is_member: None,
        is_user_deleted: None,
    };
    Slack {
        conversations: HashMap::from([(TEST_CHANNEL_ID.to_string(), mocked_channel)]),
        users: HashMap::new(),
    }
}

fn get_base_event_message() -> MessageEvent {
    MessageEvent {
        r#type: "message".to_string(),
        channel: TEST_CHANNEL_ID.to_string(),
        user: Some("user".to_string()),
        text: Some(TEST_MESSAGE_TEXT.to_string()),
        subtype: None,
        thread_ts: None,
        deleted_ts: None,
        message: None,
        previous_message: None,
        ts: TEST_MESSAGE_TS.to_string(),
    }
}

fn get_deleted_message_event() -> MessageEvent {
    MessageEvent {
        r#type: "message".to_string(),
        channel: TEST_CHANNEL_ID.to_string(),
        user: Some("user".to_string()),
        text: Some(TEST_MESSAGE_TEXT.to_string()),
        subtype: Some("message_deleted".to_string()),
        thread_ts: None,
        deleted_ts: Some(TEST_MESSAGE_TS.to_string()),
        message: None,
        previous_message: None,
        ts: "9999999.9999".to_string(),
    }
}

fn get_messages_from_channel(slack_instance: &Slack) -> &HashMap<String, BaseMessage> {
    slack_instance.conversations[TEST_CHANNEL_ID]
        .messages
        .as_ref()
        .unwrap()
}

#[test]
fn test_message_send_event() {
    let mut slack_instance = get_mocked_slack_instance();
    assert!(get_messages_from_channel(&slack_instance).is_empty());
    let event = get_base_event_message();
    assert_eq!(event.handle_event(&mut slack_instance), ());
    assert!(get_messages_from_channel(&slack_instance).contains_key(TEST_MESSAGE_TS));
    assert_eq!(
        get_messages_from_channel(&slack_instance)
            .get(TEST_MESSAGE_TS)
            .unwrap()
            .get_text(),
        TEST_MESSAGE_TEXT
    );
}
