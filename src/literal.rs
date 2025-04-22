use std::borrow::Cow;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Literal<'a> {
    Num(f64),
    Bool(bool),
    String(Cow<'a, str>),
}

impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Num(n) => n.fmt(f),
            Literal::Bool(b) => b.fmt(f),
            Literal::String(s) => s.fmt(f),
        }
    }
}

impl Literal<'_> {
    pub fn into_owned(self) -> Literal<'static> {
        match self {
            Literal::Num(n) => Literal::Num(n),
            Literal::Bool(b) => Literal::Bool(b),
            Literal::String(s) => Literal::String(s.into_owned().into()),
        }
    }
}
