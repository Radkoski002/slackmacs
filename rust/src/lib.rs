use emacs::{defun, Env, Result, Value};
use std::thread::{self, sleep};
use std::time::Duration;

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

//Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module(name = "slackmacs-module-rs", defun_prefix = "slackmacs")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

// Define a function callable by Lisp code.
#[defun]
fn say_hello(env: &Env, mut name: String) -> Result<Value<'_>> {
    name = name.replace("\n", " ");
    env.message(&format!("Hello, {}!", name))
}

#[defun]
fn change_case(_: &Env, name: String) -> Result<String> {
    let mut char_vec: Vec<char> = name.chars().collect();
    char_vec = char_vec
        .iter()
        .map(|char| {
            if char.is_uppercase() {
                char.to_ascii_lowercase()
            } else {
                char.to_ascii_uppercase()
            }
        })
        .collect();
    let result: String = char_vec.into_iter().collect();
    Ok(result)
}

#[defun]
fn simulate_long_operation(env: &Env) -> Result<Value<'_>> {
    let long_operation_thread = thread::spawn(move || {
        sleep(Duration::from_secs(120));
    });

    long_operation_thread.join().unwrap();
    env.message("Long operation done")
}

#[defun]
fn simulate_error() -> Result<String> {
    sleep(Duration::from_secs(1));
    panic!("Something went wrong");
}

#[defun]
fn benchmark_func(env: &Env) -> Result<String> {
    let buffer_str = env.call("buffer-string", &[])?;
    let updated_buffer_str = env.call("upcase", &[buffer_str])?.into_rust::<String>()?;
    Ok(updated_buffer_str)
}

#[defun]
fn benchmark_func_rust(env: &Env) -> Result<String> {
    let buffer_str = env
        .call("buffer-string", &[])?
        .into_rust::<String>()
        .unwrap();
    let updated_buffer_str = buffer_str.to_uppercase();
    Ok(updated_buffer_str)
}
