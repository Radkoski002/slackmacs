use emacs::{defun, Env, Result, Value};

#[defun]
fn say_hello_2(env: &Env, mut name: String) -> Result<Value<'_>> {
    name = name.replace("\n", " ");
    env.message(&format!("Hello, {}!", name))
}

pub fn return_string() -> String {
    "Hello, ".to_string()
}
