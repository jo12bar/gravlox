use std::fmt;
use std::rc::Rc;

use crate::LineNum;
use crate::literal::Literal;
use crate::token_type::TokenType;

#[derive(Debug, Clone)]
pub struct Token {
    typ: TokenType,
    lexeme: String,
    literal: Option<Rc<dyn Literal>>,
    line: LineNum,
}

impl Token {
    pub fn new(
        typ: TokenType,
        lexeme: impl ToString,
        literal: Option<Rc<dyn Literal>>,
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

    #[inline]
    pub fn typ(&self) -> TokenType {
        self.typ
    }

    #[inline]
    pub fn literal(&self) -> Option<&Rc<dyn Literal>> {
        self.literal.as_ref()
    }

    #[inline]
    pub fn line(&self) -> LineNum {
        self.line
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
