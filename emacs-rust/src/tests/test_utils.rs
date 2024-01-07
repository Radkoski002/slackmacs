use crate::{
    api_types::Slack,
    conversation_list::populate_conversations,
    messages_list::{populate_messages, populate_replies},
    users_list::populate_users,
};

use std::fs;

pub fn read_mocked_json_file(file_name: &str) -> String {
    fs::read_to_string(format!("../__mocks__/{}.json", file_name)).unwrap()
}

pub fn populate_mocked_conversations(slack_instance: &mut Slack) {
    let conversations_mock = read_mocked_json_file("conversations");
    match populate_conversations(conversations_mock, slack_instance) {
        Ok(()) => (),
        Err(error) => {
            println!("Error: {}", error);
        }
    };
}

pub fn populate_mocked_users(slack_instance: &mut Slack) {
    let users_mock = read_mocked_json_file("users");
    match populate_users(users_mock, slack_instance) {
        Ok(()) => (),
        Err(error) => {
            println!("Error: {}", error);
        }
    };
}

fn populate_mocked_messages(slack_instance: &mut Slack) {
    let messages_mock = read_mocked_json_file("messages");
    match populate_messages(messages_mock, "C00000001".to_string(), slack_instance) {
        Ok(()) => (),
        Err(error) => {
            println!("Error: {}", error);
        }
    };
}

fn populate_mocked_replies(slack_instance: &mut Slack) {
    let replies_mock = read_mocked_json_file("replies");
    match populate_replies(
        replies_mock,
        "C00000001".to_string(),
        "1698435687.161709".to_string(),
        slack_instance,
    ) {
        Ok(()) => (),
        Err(error) => {
            println!("Error: {}", error);
        }
    };
}

pub fn get_mocked_slackmacs_instance() -> Slack {
    let mut slack_instance = Slack::default();
    populate_mocked_conversations(&mut slack_instance);
    populate_mocked_users(&mut slack_instance);
    populate_mocked_messages(&mut slack_instance);
    populate_mocked_replies(&mut slack_instance);
    slack_instance
}
