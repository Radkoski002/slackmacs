use emacs::{defun, Env, Result, Value};
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::thread::sleep;
use std::time::{Duration, Instant};

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module(name = "emacs-slack", defun_prefix = "em-slack")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Done loading!")
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
    sleep(Duration::from_secs(120));
    env.message("Long operation done")
}

#[defun]
fn simulate_error() -> Result<String> {
    sleep(Duration::from_secs(1));
    panic!("Something went wrong");
}

#[defun]
fn benchmark_func(_: &Env, name: String) -> Result<String> {
    let now = Instant::now();
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
    let elapsed = now.elapsed().as_secs_f64();
    let mut data_file = OpenOptions::new()
        .append(true)
        .open("/home/radkoski/Coding/Studia/INZ/emacs-slack/benchmarks.txt")
        .expect("cannot open file");

    // Write to a file
    data_file
        .write(format!("{:.10?} ", elapsed).as_bytes())
        .expect("write failed");
    Ok(result)
}
