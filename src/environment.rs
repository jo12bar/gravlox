use rustc_hash::FxHashMap;

use crate::{interpreter::Value, runtime_error::RuntimeError, token::Token};

/// An environment for the interpreter to store computed values in.
#[derive(Debug, Default)]
pub struct Environment {
    values: FxHashMap<String, Value<'static>>,
    /// The enclosing (i.e. parent) environment
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    /// Create a new environment that is _enclosed_ by another parent environment.
    pub fn new_enclosed_by(enclosing_environment: Box<Environment>) -> Environment {
        Self {
            enclosing: Some(enclosing_environment),
            ..Default::default()
        }
    }

    /// Take the enclosing environment, if it exists.
    #[inline]
    pub const fn take_enclosing_environment(&mut self) -> Option<Box<Environment>> {
        self.enclosing.take()
    }

    /// Bind a new name to a value.
    ///
    /// Basically a thin wrapper over [`std::collections::HashMap::insert()`].
    /// If the environment did not have this value defined, then [`None`] is returned.
    /// If the environment did have this value defined, then the value is updated with
    /// the new value, and the old value is returned.
    ///
    /// Note that new variables are always declared in _this_ environment, not
    /// any enclosing environments that may or may not exist. Put another way,
    /// new variables are always defined in the innermost scope.
    pub fn define(&mut self, name: impl ToString, value: Value<'static>) -> Option<Value<'static>> {
        self.values.insert(name.to_string(), value)
    }

    /// Look up and return an immutable reference to a variable definition.
    ///
    /// If the variable isn't found in this environment, and this environment
    /// is enclosed by another, then it will look it up in the enclosing
    /// environment.
    ///
    /// Returns a runtime error if the variable does not exist.
    pub fn get<'a>(&'a self, name: &Token<'_>) -> Result<&'a Value<'static>, RuntimeError> {
        if let Some(v) = self.values.get(name.lexeme()) {
            Ok(v)
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.get(name)
        } else {
            Err(RuntimeError::new(
                name.clone(),
                format!("Undefined variable '{}'", name.lexeme()),
            ))
        }
    }

    /// Set a pre-defined variable to something.
    ///
    /// If the variable isn't found in this environment, and this environment
    /// is enclosed by another, then it will look it up in the enclosing
    /// environment and set it.
    ///
    /// Returns a runtime error if the variable does not exist.
    pub fn assign(&mut self, name: &Token<'_>, value: Value<'static>) -> Result<(), RuntimeError> {
        if let Some(v) = self.values.get_mut(name.lexeme()) {
            *v = value;
            Ok(())
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.assign(name, value)
        } else {
            Err(RuntimeError::new(
                name.clone(),
                format!("Undefined variable '{}'", name.lexeme()),
            ))
        }
    }
}
