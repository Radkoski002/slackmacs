use crate::{api_types::Slack, tests::test_utils::get_mocked_slackmacs_instance};
#[test]
fn test_slack_init() {
    let slack_instance = Slack::default();
    assert_eq!(slack_instance.conversations.len(), 0);
    assert_eq!(slack_instance.users.len(), 0)
}

#[test]
fn test_slack_instance_populate() {
    let instance = get_mocked_slackmacs_instance();
    let conversations = instance.conversations;
    let users = instance.users;
    assert_eq!(conversations.len(), 7);
    assert_eq!(users.len(), 3);
    let messages = conversations
        .get("C00000001")
        .unwrap()
        .messages
        .as_ref()
        .unwrap();
    assert_eq!(messages.len(), 4);
    let replies = messages
        .get("1698435687.161709")
        .unwrap()
        .replies
        .as_ref()
        .unwrap();
    assert_eq!(replies.len(), 3)
}
