use crate::api_types::Slack;
#[test]
fn test_slack_init() {
    let slack_instance = Slack::default();
    assert_eq!(slack_instance.conversations.len(), 0);
    assert_eq!(slack_instance.users.len(), 0)
}
