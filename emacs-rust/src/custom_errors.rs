// elisp
emacs::define_errors! {
    api_error "There was an error with slack API" (error)
    param_error "Invalid parameter" (wrong_type_argument)
    buffer_error "Invalid buffer" (wrong_type_argument)
}

// rust
pub struct ApiError {
    pub message: String,
}

impl std::fmt::Display for ApiError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Api error: {}", self.message)
    }
}

pub struct ParamError {
    pub message: String,
}

impl std::fmt::Display for ParamError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Param error: {}", self.message)
    }
}
