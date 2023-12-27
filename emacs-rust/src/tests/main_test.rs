use crate::start;

#[test]
fn test_start() {
    let slack_instance = start();
    let slack_instance = slack_instance.unwrap();
    assert_eq!(slack_instance.conversations.len(), 0);
    assert_eq!(slack_instance.users.len(), 0)
}
