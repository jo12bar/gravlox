use std::fmt;

use crate::LineNum;
use crate::literal::Literal;
use crate::token_type::TokenType;

pub struct Token {
    typ: TokenType,
    lexeme: String,
    literal: Option<Box<dyn Literal>>,
    line: LineNum,
}

impl Token {
    pub fn new(
        typ: TokenType,
        lexeme: impl ToString,
        literal: Option<Box<dyn Literal>>,
        line: LineNum,
    ) -> Token {
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
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Token")
            .field("typ", &self.typ)
            .field("lexeme", &self.lexeme)
            .field("literal", &self.literal)
            .field("line", &self.line)
            .finish()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(literal) = &self.literal {
            write!(f, "{:?} {} {}", self.typ, self.lexeme, literal)
        } else {
            write!(f, "{:?} {}", self.typ, self.lexeme)
        }
    }
}
