use std::fmt;

use crate::token::Token;

#[derive(Debug)]
pub struct RuntimeError {
    pub token: Token,
    pub message: String,
}

impl RuntimeError {
    pub fn new(token: Token, message: String) -> Self {
        Self { token, message }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Runtime error at token `{}`: {}",
            self.token, self.message
        )
    }
}

impl std::error::Error for RuntimeError {}
