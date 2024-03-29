use api_types::Slack;
use emacs::{defun, Env, Result};

pub mod api_types;

mod conversation;
mod conversation_list;
mod custom_errors;
pub mod helpers;
mod message;
mod messages_list;
mod user;
mod users_list;
mod websocket;

#[cfg(test)]
mod tests;

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

//Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module(
    name = "slackmacs-module-rs",
    defun_prefix = "slackmacs",
    separator = "/"
)]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

#[defun(user_ptr)]
pub fn start() -> Result<Slack> {
    Ok(Slack::default())
}
