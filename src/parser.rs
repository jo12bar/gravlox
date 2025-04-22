use std::fmt;

use crate::{expr::Expr, literal::Literal, token::Token, token_type::TokenType, Lox};

/// A recursive-descent parser.
#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
}

impl Parser<'_> {
    pub fn new(tokens: Vec<Token<'_>>) -> Parser {
        Parser { tokens, current: 0 }
    }

    /// Parse the input tokens into an AST.
    pub fn parse(&mut self, lox: &mut Lox) -> Result<Expr, ParserError> {
        self.expression(lox)
    }

    /// Keep discarding tokens until we hit a statement boundary.
    ///
    /// Useful for recovering from [`ParserError`]s.
    fn synchronize(&mut self) {
        use TokenType::*;

        self.advance();

        while !self.is_at_end() {
            if self.previous().typ() == Semicolon {
                return;
            }

            if matches!(
                self.peek().typ(),
                Class | Fun | Var | For | If | While | Print | Return
            ) {
                return;
            }

            self.advance();
        }
    }

    /// Check if the current token matches _any_ of the given types.
    ///
    /// If so, consumes the token and returns `true`. Otherwise, returns `false`
    /// and leaves the current token alone.
    fn match_tokens<I>(&mut self, tokens: I) -> bool
    where
        I: IntoIterator<Item = TokenType>,
    {
        for token in tokens.into_iter() {
            if self.check(token) {
                self.advance();
                return true;
            }
        }

        false
    }

    /// Try to consume a token, or return an error if the token doesn't match
    /// the next token.
    fn consume(
        &mut self,
        token_type: TokenType,
        err_msg: &'static str,
        lox: &mut Lox,
    ) -> Result<&Token, ParserError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(ParserError::new_report(self.peek(), err_msg, lox))
        }
    }

    /// Returns `true` if the current token is of a given type.
    ///
    /// Never consumes the token - only peeks at it.
    ///
    /// Returns `false` if at the end of the file.
    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().typ() == token_type
    }

    /// Consumes the current token and returns it.
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    /// Returns `true` if at the end of the token stream.
    fn is_at_end(&self) -> bool {
        self.peek().typ() == TokenType::Eof
    }

    /// Peek at the _current_ token without advancing the parser.
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    /// Peek at the _previous_ token without advancing the parser.
    ///
    /// Will probably panic if `self.current == 0`.
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse an [expression][Expr].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// expression     → comma ;
    /// ```
    fn expression(&mut self, lox: &mut Lox) -> Result<Expr, ParserError> {
        self.comma(lox)
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse a comma [expression][Expr].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// comma          → equality ( "," equality )* ;
    /// ```
    fn comma(&mut self, lox: &mut Lox) -> Result<Expr, ParserError> {
        let mut expr = self.equality(lox)?.into_owned();

        while self.match_tokens([TokenType::Comma]) {
            let operator = self.previous().clone().into_owned();
            let right = self.equality(lox)?.into_owned();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse an equality [expression][Expr].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    /// ```
    fn equality(&mut self, lox: &mut Lox) -> Result<Expr, ParserError> {
        let mut expr = self.comparison(lox)?.into_owned();

        while self.match_tokens([TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone().into_owned();
            let right = self.comparison(lox)?.into_owned();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse a comparison [expression][Expr].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// comparison       → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    /// ```
    fn comparison(&mut self, lox: &mut Lox) -> Result<Expr, ParserError> {
        let mut expr = self.term(lox)?.into_owned();

        while self.match_tokens([
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous().clone().into_owned();
            let right = self.term(lox)?.into_owned();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse a term [expression][Expr].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// term           → factor ( ( "-" | "+" ) factor )* ;
    /// ```
    fn term(&mut self, lox: &mut Lox) -> Result<Expr, ParserError> {
        let mut expr = self.factor(lox)?.into_owned();

        while self.match_tokens([TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone().into_owned();
            let right = self.factor(lox)?.into_owned();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse a factor [expression][Expr].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// factor         → unary ( ( "/" | "*" ) unary )* ;
    /// ```
    fn factor(&mut self, lox: &mut Lox) -> Result<Expr, ParserError> {
        let mut expr = self.unary(lox)?.into_owned();

        while self.match_tokens([TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone().into_owned();
            let right = self.unary(lox)?.into_owned();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse an unary [expression][Expr].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// unary          → ( "!" | "-" ) unary
    ///                | primary ;
    /// ```
    fn unary(&mut self, lox: &mut Lox) -> Result<Expr, ParserError> {
        if self.match_tokens([TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone().into_owned();
            let right = self.unary(lox)?.into_owned();
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }

        self.primary(lox)
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse a primary [expression][Expr].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// primary        → NUMBER | STRING | "true" | "false" | "nil"
    ///                | "(" expression ")" ;
    /// ```
    fn primary(&mut self, lox: &mut Lox) -> Result<Expr, ParserError> {
        if self.match_tokens([TokenType::False]) {
            return Ok(Expr::Literal(Some(Literal::Bool(false))));
        }
        if self.match_tokens([TokenType::True]) {
            return Ok(Expr::Literal(Some(Literal::Bool(true))));
        }
        if self.match_tokens([TokenType::Nil]) {
            return Ok(Expr::Literal(None));
        }

        if self.match_tokens([TokenType::Number, TokenType::String]) {
            return Ok(Expr::Literal(self.previous().literal().cloned()));
        }

        if self.match_tokens([TokenType::LeftParen]) {
            let expr = Box::new(self.expression(lox)?.into_owned());
            self.consume(TokenType::RightParen, "Expect ')' after expression.", lox)?;
            return Ok(Expr::Grouping(expr));
        }

        Err(ParserError::new_report(
            self.peek(),
            "Expect expression.",
            lox,
        ))
    }
}

#[derive(Debug)]
pub struct ParserError(&'static str);

impl ParserError {
    /// Create a new parser error, and immediately report it through the Lox interpreter.
    ///
    /// Original book name: `static void Parser.error(Token token, String message)`
    fn new_report(token: &Token, message: &'static str, lox: &mut Lox) -> Self {
        lox.token_error(token, message);
        ParserError(message)
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Token error: {}", self.0)
    }
}

impl std::error::Error for ParserError {}
