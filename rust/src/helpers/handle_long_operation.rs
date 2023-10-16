use std::{sync::mpsc::Receiver, thread, time::Duration};

use emacs::{Env, Result};

pub fn handle_long_operation(env: &Env, rx: Receiver<i32>, message: Option<String>) -> Result<()> {
    let loading_message = message.unwrap_or("Loading".to_string());
    while Ok(1) != rx.try_recv() {
        thread::sleep(Duration::from_millis(100));
        env.message(&format!("{}", loading_message))?;
    }
    env.message("Done")?;
    Ok(())
}
