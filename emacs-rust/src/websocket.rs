use emacs::{defun, Result};

use crate::api_types::{Event, EventType, MessageEvent, Slack};

#[defun]
pub fn handle_events(events_json: String, slack_instance: &mut Slack) -> Result<()> {
    let parsed_json = serde_json::from_str::<serde_json::Value>(events_json.as_str()).unwrap();
    let parsed_json = parsed_json.as_array().unwrap();
    for event in parsed_json {
        let event_type = event["type"].as_str().unwrap();
        let parsed_event = match event_type {
            "message" => {
                let parsed_event =
                    serde_json::from_str::<MessageEvent>(event.to_string().as_str()).unwrap();
                EventType::MessageEvent(parsed_event)
            }
            _ => {
                let parsed_event =
                    serde_json::from_str::<Event>(event.to_string().as_str()).unwrap();
                EventType::Generic(parsed_event)
            }
        };
        match parsed_event {
            EventType::MessageEvent(message_event) => {
                message_event.handle_event(slack_instance);
            }
            EventType::Generic(_) => continue,
        }
    }
    Ok(())
}
