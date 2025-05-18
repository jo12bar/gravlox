use std::fmt;

use crate::{
    Lox,
    ast::{Expr, Stmt},
    literal::Literal,
    token::Token,
    token_type::TokenType,
};

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
    ///
    /// Any statements that failed to be parsed will be returned as `None`.
    /// The passed-in [Lox] instance will be used for reporting parsing errors.
    /// When interpreting, make sure to avoid interpreting any statements that
    /// failed to parse (i.e. returned as `None`), whether that involves
    /// just interpreting all other statements or not interpreting anything in
    /// the first place.
    pub fn parse(&mut self, lox: &mut Lox) -> Vec<Option<Stmt<'static>>> {
        let mut statements = vec![];

        while !self.is_at_end() {
            statements.push(self.declaration(lox).map(|d| d.into_owned()));
        }

        statements
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
    /// Parse a "declaration" [statement][Stmt].
    ///
    /// Recovers from errors using [Parser::synchronize()]. Returns `None` if
    /// parsing resulted in an error.
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// declaration    → varDecl
    ///                | statement ;
    /// ```
    fn declaration(&mut self, lox: &mut Lox) -> Option<Stmt<'static>> {
        if self.match_tokens([TokenType::Var]) {
            self.var_declaration(lox)
        } else {
            self.statement(lox)
        }
        .ok()
        .map(|s| s.into_owned())
        .or_else(|| {
            self.synchronize();
            None
        })
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse a variable declaration [statement][Stmt].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
    /// ```
    fn var_declaration(&mut self, lox: &mut Lox) -> Result<Stmt<'_>, ParserError> {
        let name = self
            .consume(TokenType::Identifier, "Expect variable name.", lox)?
            .clone()
            .into_owned();

        let mut initializer = None;
        if self.match_tokens([TokenType::Equal]) {
            initializer = Some(self.expression(lox)?.into_owned());
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
            lox,
        )?;

        Ok(Stmt::Var { name, initializer })
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse a [statement][Stmt].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// statement      → exprStmt
    ///                | printStmt ;
    /// ```
    fn statement(&mut self, lox: &mut Lox) -> Result<Stmt<'_>, ParserError> {
        if self.match_tokens([TokenType::If]) {
            self.if_statement(lox)
        } else if self.match_tokens([TokenType::Print]) {
            self.print_statement(lox)
        } else if self.match_tokens([TokenType::LeftBrace]) {
            self.block(lox).map(|statements| Stmt::Block { statements })
        } else {
            self.expression_statement(lox)
        }
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse a print [statement][Stmt].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// printStmt      → "print" expression ";" ;
    /// ```
    fn print_statement(&mut self, lox: &mut Lox) -> Result<Stmt<'static>, ParserError> {
        let value = self.expression(lox)?.into_owned();
        self.consume(TokenType::Semicolon, "Expect ';' after value.", lox)?;
        Ok(Stmt::Print(value))
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse an if [statement][Stmt].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// ifStmt         → "if" "(" expression ")" statement
    ///                  ( "else" statement )? ;
    /// ```
    fn if_statement(&mut self, lox: &mut Lox) -> Result<Stmt<'static>, ParserError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.", lox)?;
        let condition = self.expression(lox)?.into_owned();
        self.consume(TokenType::RightParen, "Expect ')' after if condition.", lox)?;

        let then_branch = self.statement(lox)?.into_owned();

        let mut else_branch = None;
        if self.match_tokens([TokenType::Else]) {
            else_branch = Some(Box::new(self.statement(lox)?.into_owned()));
        }

        Ok(Stmt::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse a block [statement][Stmt].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// block          → "{" declaration* "}" ;
    /// ```
    fn block(&mut self, lox: &mut Lox) -> Result<Vec<Stmt<'static>>, ParserError> {
        let mut statements = vec![];

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            // to help with error recovery, filter out all statements that resulted in an error while parsing
            if let Some(stmt) = self.declaration(lox).map(|d| d.into_owned()) {
                statements.push(stmt);
            }
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.", lox)?;

        Ok(statements)
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse an expression [statement][Stmt].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// exprStmt       → expression ";" ;
    /// ```
    fn expression_statement(&mut self, lox: &mut Lox) -> Result<Stmt<'static>, ParserError> {
        let value = self.expression(lox)?.into_owned();
        self.consume(TokenType::Semicolon, "Expect ';' after value.", lox)?;
        Ok(Stmt::Expression(value))
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse an [expression][Expr].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// expression     → assignment ;
    /// ```
    fn expression(&mut self, lox: &mut Lox) -> Result<Expr, ParserError> {
        self.assignment(lox)
    }

    #[allow(rustdoc::invalid_rust_codeblocks)]
    /// Parse an assignment [expression][Expr].
    ///
    /// Grammar:
    ///
    /// ```ignore
    /// assignment     → IDENTIFIER "=" assignment
    ///                | comma ;
    /// ```
    fn assignment(&mut self, lox: &mut Lox) -> Result<Expr<'static>, ParserError> {
        let expr = self.comma(lox)?.into_owned();

        if self.match_tokens([TokenType::Equal]) {
            let equals = self.previous().clone().into_owned();
            let value = self.assignment(lox)?;

            if let Expr::Var { name } = expr {
                return Ok(Expr::Assign {
                    name,
                    value: Box::new(value),
                });
            }

            // we report an error if th eleft-hand side isn't a valid assignment target,
            // but we don't throw it because the parser isn't in a confused
            // state where we need to go into panic mode and synchronize.
            lox.token_error(&equals, "Invalid assignment target.");
        }

        Ok(expr)
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

        if self.match_tokens([TokenType::Identifier]) {
            return Ok(Expr::Var {
                name: self.previous().clone(),
            });
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
