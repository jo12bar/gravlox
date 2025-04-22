use std::fmt;

use crate::LineNum;
use crate::literal::Literal;
use crate::token_type::TokenType;

#[derive(Debug, Clone)]
pub struct Token<'a> {
    typ: TokenType,
    lexeme: String,
    literal: Option<Literal<'a>>,
    line: LineNum,
}

impl Token<'_> {
    pub fn new(
        typ: TokenType,
        lexeme: impl ToString,
        literal: Option<Literal<'_>>,
        line: LineNum,
    ) -> Token<'_> {
        Token {
            typ,
            lexeme: lexeme.to_string(),
            literal,
            line,
        }
    }

    #[inline]
    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }

    #[inline]
    pub fn typ(&self) -> TokenType {
        self.typ
    }

    #[inline]
    #[allow(clippy::borrowed_box)]
    pub fn literal(&self) -> Option<&Literal<'_>> {
        self.literal.as_ref()
    }

    #[inline]
    pub fn line(&self) -> LineNum {
        self.line
    }

    pub fn into_owned(self) -> Token<'static> {
        Token {
            literal: self.literal.map(|l| l.into_owned()),
            ..self
        }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(literal) = &self.literal {
            write!(f, "{:?} {} {}", self.typ, self.lexeme, literal)
        } else {
            write!(f, "{:?} {}", self.typ, self.lexeme)
        }
    }
}
