use api_types::Slack;
use emacs::{defun, Env, IntoLisp, Result, Value};

mod api_types;

mod conversation;
mod conversation_list;
mod custom_errors;
mod helpers;
mod message;
mod messages_list;
mod user;
mod users_list;
mod websocket;

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
fn start() -> Result<Slack> {
    Ok(Slack::default())
}

#[defun]
fn get_slack_channels(slack_instance: &mut Slack, env: &Env) -> Result<()> {
    let channels = &slack_instance.conversations;
    for (name, _) in channels {
        env.message(name)?;
    }
    Ok(())
}

#[defun]
fn test(env: &Env, lambda: Value) -> Result<()> {
    let string = "Hello, world!".to_string().into_lisp(env)?;
    lambda.call((string,))?;
    Ok(())
}
