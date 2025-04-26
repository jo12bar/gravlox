use rustc_hash::FxHashMap;

use crate::{interpreter::Value, runtime_error::RuntimeError, token::Token};

/// An environment for the interpreter to store computed values in.
#[derive(Debug, Default)]
pub struct Environment {
    values: FxHashMap<String, Value<'static>>,
}

impl Environment {
    /// Bind a new name to a value.
    ///
    /// Basically a thin wrapper over [`std::collections::HashMap::insert()`].
    /// If the environment did not have this value defined, then [`None`] is returned.
    /// If the environment did have this value defined, then the value is updated with
    /// the new value, and the old value is returned.
    pub fn define(&mut self, name: impl ToString, value: Value<'static>) -> Option<Value<'static>> {
        self.values.insert(name.to_string(), value)
    }

    /// Look up and return an immutable reference to a variable definition.
    ///
    /// Returns a runtime error if the variable does not exist.
    pub fn get<'a>(&'a self, name: &Token<'_>) -> Result<&'a Value<'static>, RuntimeError> {
        self.values.get(name.lexeme()).ok_or_else(|| {
            RuntimeError::new(
                name.clone(),
                format!("Undefined variable '{}'", name.lexeme()),
            )
        })
    }
}
