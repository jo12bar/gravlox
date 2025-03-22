use std::rc::Rc;
use std::sync::LazyLock;

use rustc_hash::FxHashMap;

use crate::{LineNum, Lox, literal::Literal, token::Token, token_type::TokenType};

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,

    start: LineNum,
    current: LineNum,
    line: LineNum,
}

impl Scanner {
    pub fn new(source: impl ToString) -> Scanner {
        Scanner {
            source: source.to_string(),
            tokens: Vec::new(),

            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn iter_tokens(&self) -> impl Iterator<Item = &Token> {
        self.tokens.iter()
    }

    pub fn scan_tokens(&mut self, lox: &mut Lox) {
        while !self.is_at_end() {
            // We are at the beginning of the next lexeme
            self.start = self.current;
            self.scan_token(lox);
        }

        self.tokens
            .push(Token::new(TokenType::Eof, "", None, self.line));
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self, lox: &mut Lox) {
        let c = self.advance();

        use TokenType::*;

        match c {
            '(' => self.add_token(LeftParen),
            ')' => self.add_token(RightParen),
            '{' => self.add_token(LeftBrace),
            '}' => self.add_token(RightBrace),
            ',' => self.add_token(Comma),
            '.' => self.add_token(Dot),
            '-' => self.add_token(Minus),
            '+' => self.add_token(Plus),
            ';' => self.add_token(Semicolon),
            '*' => self.add_token(Star),

            '!' => {
                if self.check_and_advance('=') {
                    self.add_token(BangEqual);
                } else {
                    self.add_token(Bang);
                }
            }
            '=' => {
                if self.check_and_advance('=') {
                    self.add_token(EqualEqual);
                } else {
                    self.add_token(Equal);
                }
            }
            '<' => {
                if self.check_and_advance('=') {
                    self.add_token(LessEqual);
                } else {
                    self.add_token(Less);
                }
            }
            '>' => {
                if self.check_and_advance('=') {
                    self.add_token(GreaterEqual);
                } else {
                    self.add_token(Greater);
                }
            }

            '/' => {
                if self.check_and_advance('/') {
                    // Comments go to the end of the line.
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.check_and_advance('*') {
                    self.block_comment();
                } else {
                    self.add_token(Slash);
                }
            }

            // Ignore whitespace
            ' ' | '\r' | '\t' => {}

            '\n' => {
                self.line += 1;
            }

            '"' => {
                self.string(lox);
            }

            c if c.is_ascii_digit() => {
                self.number();
            }

            // Assume that any lexeme starting with a letter or underscore is
            // an identifier. The Scanner::identifier() function will determine
            // if it's an actual identifier or a keyword.
            c if c.is_ascii_alphabetic() || c == '_' => {
                self.identifier();
            }

            _ => {
                lox.error_with_linenum(self.line, "Unexpected character.");
            }
        }
    }

    /// Scan a block comment, `/* like this */`
    fn block_comment(&mut self) {
        // if we're here, we've already scanned a /*
        #[allow(clippy::nonminimal_bool)]
        while !self.is_at_end() && !(self.peek() == '*' && self.peek_next() == '/') {
            // handle nested comment blocks by recursing
            if self.check_and_advance('/') && self.check_and_advance('*') {
                self.block_comment();
            }

            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        // consume the final */, unless if we're at the end of the file, in which case
        // the entire file is just treated as commented-out.
        if !self.is_at_end() {
            self.advance();
            if !self.is_at_end() {
                self.advance();
            }
        }
    }

    /// Scan a string, like `"something surrounded with quotes, and maybe with newlines"`.
    fn string(&mut self, lox: &mut Lox) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            lox.error_with_linenum(self.line, "Unterminated string.");
            return;
        }

        // The closing ".
        self.advance();

        // Trim the surrounding quotes
        let str_value = self
            .get_source_substring(self.start + 1, self.current - 1)
            .to_string();
        self.add_token_lit(TokenType::String, Some(Rc::new(str_value)));
    }

    /// Scan a number, like `103` or `0.23034` or `420.69`.
    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Look for a fractional part
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // Consume the "."
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let num = self
            .get_source_substring(self.start, self.current)
            .parse::<f64>()
            .expect("parsing number should succeed since we already checked its format");
        self.add_token_lit(TokenType::Number, Some(Rc::new(num)));
    }

    /// Scan an identifier, using C's identifier rules.
    fn identifier(&mut self) {
        let check_ident_char = |c: char| c.is_ascii_alphabetic() || c == '_' || c.is_ascii_digit();

        while check_ident_char(self.peek()) {
            self.advance();
        }

        let text = self.get_source_substring(self.start, self.current);
        let token_type = KEYWORDS.get(text).unwrap_or(&TokenType::Identifier);

        self.add_token(*token_type);
    }

    /// Advance the scanner's cursor.
    ///
    /// Panics if called when the scanner is already at the end of the file.
    fn advance(&mut self) -> char {
        let c = self
            .get_source_char_at(self.current)
            .expect("advance should only be called when not at the end of the file");
        self.current += 1;
        c
    }

    /// Checks if the current character matches an expected character.
    ///
    /// If it does, returns `true` and advances the cursor. If it doesn't,
    /// or if at the EOF, returns `false` and _doesn't_ advance.
    ///
    /// `Scanner.match()` from the original interpreter.
    ///
    /// Panics if called when the scanner is already at the end of the file.
    fn check_and_advance(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        let cur_char = self
            .get_source_char_at(self.current)
            .expect("check_and_advance should only be called when not at the end of the file");

        if cur_char != expected {
            return false;
        }

        self.current += 1;
        true
    }

    /// Check the current character _without_ advancing the scanner's cursor.
    ///
    /// Always returns the null character (`'\0'`) if the scanner is at the end
    /// of the file.
    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        // Unwrap should be fine here since we already checked if we're at the end of the file.
        self.get_source_char_at(self.current).unwrap()
    }

    /// Check the _next_ character _without_ advancing the scanner's cursor.
    ///
    /// Always returns the null character (`'\0'`) if the scanner is at the end
    /// of the file, or if the next character is the end of the file.
    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        // Unwrap should be fine here since we already checked if we're at the end of the file.
        self.get_source_char_at(self.current + 1).unwrap()
    }

    fn add_token(&mut self, typ: TokenType) {
        self.add_token_lit(typ, None);
    }

    fn add_token_lit(&mut self, typ: TokenType, literal: Option<Rc<dyn Literal>>) {
        let lexeme = self.get_source_substring(self.start, self.current);
        self.tokens
            .push(Token::new(typ, lexeme, literal, self.line));
    }

    #[inline]
    fn get_source_char_at(&self, pos: usize) -> Option<char> {
        self.source.chars().nth(pos)
    }

    fn get_source_substring(&self, start: usize, end: usize) -> &str {
        let unpack_index = |(index, _char)| index;

        let mut indices = self.source.char_indices();

        let start_index = indices.nth(start).map_or(self.source.len(), unpack_index);
        let end_index = indices
            .nth(end - start - 1)
            .map_or(self.source.len(), unpack_index);

        &self.source[start_index..end_index]
    }
}

static KEYWORDS: LazyLock<FxHashMap<&'static str, TokenType>> = LazyLock::new(|| {
    use TokenType::*;
    [
        ("and", And),
        ("class", Class),
        ("else", Else),
        ("false", False),
        ("for", For),
        ("fun", Fun),
        ("if", If),
        ("nil", Nil),
        ("or", Or),
        ("print", Print),
        ("return", Return),
        ("super", Super),
        ("this", This),
        ("true", True),
        ("var", Var),
        ("while", While),
    ]
    .into_iter()
    .collect()
});
