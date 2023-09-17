use emacs::{Env, Result};

mod api_types;
mod helpers;

mod api_helpers;
mod conversation;
mod conversation_list;
mod custom_errors;
mod users_list;

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
