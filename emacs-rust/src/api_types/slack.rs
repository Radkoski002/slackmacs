use std::collections::HashMap;

use super::{Conversation, User};

pub struct Slack {
    pub conversations: HashMap<String, Conversation>,
    pub users: HashMap<String, User>,
}

impl Default for Slack {
    fn default() -> Self {
        Self {
            conversations: HashMap::new(),
            users: HashMap::new(),
        }
    }
}
