use std::collections::HashMap;

use log::error;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Lexer pointer is out of bounds.")]
    OutOfBounds,
    #[error("Lexing failed due to one or more errors.")]
    LexingFailed,
}

type Result<T> = std::result::Result<T, LexerError>;

#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq)]
pub enum Lexeme {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    LEFT_BRACE,
    RIGHT_BRACE,
    VAL,
    FUNCTION,
    FOR,
    IN,
    IF,
    THEN,
    ELSE,
    FALSE,
    TRUE,
    AND,
    NOT,
    OR,
    EQ,
    NEQ,
    WHILE,
    NUMBER(f64),
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    GREATER_THAN,
    LESS_THAN,
    GREATER_EQUAL,
    LESS_EQUAL,
    STRING(String),
    IDENTIFIER(String),
    XOR,
    MODULO,
    ASSIGN,
    SEMICOLON,
    DOT,
    COMMA,
    COLON,
    UNDEFINED,
}

/// This is the span of the lexeme in the text.
#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    lexeme: Lexeme,
    span: Span,
    /// The line and column.
    position: (usize, usize),
}

pub struct Lexer {
    text: String,
    /// The line and column that the lexer is currently on.
    position: (usize, usize),
    /// if we have errored at least once.
    errored: bool,
    /// this is the base of the what the lexer is currently pointing to.
    /// analogue of the x86 stack base pointer.
    base_pointer: usize,
    /// this is actually what the lexer is currently pointing to.
    pointer: usize,
}

impl Lexer {
    pub fn new(string: &str) -> Self {
        Self {
            text: string.to_owned(),
            position: (1, 1),
            errored: false,
            base_pointer: 0,
            pointer: 0,
        }
    }

    fn current_char(&self) -> Result<char> {
        self.text
            .chars()
            .nth(self.pointer)
            .ok_or(LexerError::OutOfBounds)
    }

    fn peek(&self, delta: Option<usize>) -> char {
        self.text
            .chars()
            .nth(self.pointer + delta.unwrap_or(1))
            .unwrap_or_default()
    }

    fn single_advance(&mut self) {
        // advance the state of the lexer.
        self.position = (self.position.0, self.position.1 + 1);
        // advance the text pointer.
        self.pointer += 1;
    }

    fn is_eof(&mut self) -> bool {
        self.pointer == self.text.len()
    }

    /// This will consume the lexer text.
    fn accept(&mut self, s: &str) -> bool {
        // get the slice that we could potentially match with.
        if s == &self.text[self.pointer..self.pointer + s.len()] {
            // increase the pointer by the sice of the accepted string.
            self.pointer += s.len();
            return true;
        }
        false
    }

    fn handle_number(&mut self, base: usize) -> Lexeme {
        // set the base to the start of the identifier.
        self.base_pointer = self.pointer;

        // advance until we reach the end of the number.
        while self.peek(Some(0)).is_numeric() || self.peek(Some(0)) == '.' {
            self.pointer += 1;
        }

        // get the slice of the number.
        let number = self.text[self.base_pointer..self.pointer].to_owned();

        if base == 10
            && let Ok(number) = number.parse::<f64>()
        {
            return Lexeme::NUMBER(number);
        }

        // do not need to worry about decimals at this point.
        if let Ok(number) = u64::from_str_radix(&number, base as _) {
            return Lexeme::NUMBER(number as f64);
        }

        // if nothing was parsed correctly return error.
        self.errored = true;
        error!(
            "invalid number (line: {}, col: {}): {}",
            self.position.0, self.position.1, number
        );

        return Lexeme::UNDEFINED;
    }

    fn handle_string(&mut self) -> Lexeme {
        // set the base to the start of the identifier.
        self.base_pointer = self.pointer;

        // advance after the first string initiator.
        self.single_advance();

        // keep consuming until we're done with the string.
        while self.peek(Some(0)) != '"' {
            // if there is a new line then report an error.
            if self.peek(Some(0)) == '\n' || self.is_eof() {
                // increase the line number and reset the column.
                // self.position = (self.position.0 + 1, 1);
                self.errored = true;
                error!(
                    "unterminated string: line: {}, col: {}",
                    self.position.0,
                    self.position.1 - 1
                );
                return Lexeme::UNDEFINED;
            }
            self.pointer += 1;
        }

        // take the string inbetween the double quotes.
        let string = self.text[self.base_pointer + 1..self.pointer].to_owned();

        // advance after the second string "initiator".
        self.single_advance();

        Lexeme::STRING(string)
    }

    fn handle_comment(&mut self) {
        // set the base to the start of the identifier.
        self.base_pointer = self.pointer;

        // keep consuming until we've found the comment.
        while !self.accept("*)") {
            // we still need to keep track of the line number.
            if self.peek(Some(0)) == '\n' {
                // increase the line number and reset the column.
                self.position = (self.position.0 + 1, 1);
            }
            self.pointer += 1;
        }

        // update the position to after the comment.
        self.position.1 += (self.pointer - self.base_pointer) + 2;
    }

    fn handle_identifier(&mut self) -> Result<Lexeme> {
        let keywords = HashMap::from([
            ("not", Lexeme::NOT),
            ("andalso", Lexeme::AND),
            ("orelse", Lexeme::OR),
            ("while", Lexeme::WHILE),
            ("for", Lexeme::FOR),
            ("if", Lexeme::IF),
            ("then", Lexeme::THEN),
            ("else", Lexeme::ELSE),
            ("val", Lexeme::VAL),
            ("fun", Lexeme::FUNCTION),
            ("mod", Lexeme::MODULO),
            ("in", Lexeme::IN),
            ("true", Lexeme::TRUE),
            ("false", Lexeme::FALSE),
        ]);

        // set the base to the start of the identifier.
        self.base_pointer = self.pointer;

        // keep consuming until we're done with the identifier.
        while self.peek(None).is_alphanumeric() || self.peek(None) == '_' {
            self.pointer += 1;
        }

        // this is the complete identifier.
        let identifier = &self.text[self.base_pointer..=self.pointer];

        // first make sure that it's not a keyword.
        if let Some(kw) = keywords.get(identifier) {
            return Ok(kw.clone());
        }

        return Ok(Lexeme::IDENTIFIER(identifier.to_owned()));
    }

    /// This will push a token to a list of tokens and modify the lexer state.
    fn accept_multicharacter_token(
        &mut self,
        text: &str,
        lexeme: Lexeme,
        tokens: &mut Vec<Token>,
    ) -> bool {
        if self.accept(text) {
            tokens.push(Token {
                lexeme,
                span: Span::new(self.pointer - text.len(), self.pointer - 1),
                position: self.position,
            });
            // update the position to after symbol.
            self.position.1 += text.len();
            return true;
        }
        false
    }

    pub fn scan(&mut self) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();

        // while we are not at the end of the file.
        while self.pointer < self.text.len() as _ {
            // update the state of the lexer if new line.
            if self.current_char()? == '\n' {
                // increase the line and reset the column.
                self.position = (self.position.0 + 1, 0);
                self.single_advance();
                continue;
            }

            // disregard whitespace.
            if self.current_char()?.is_whitespace() {
                self.single_advance();
                continue;
            }

            // handles multiline comments.
            if self.accept("(*") {
                self.handle_comment();
                continue;
            }

            // handle a string when we find an opening.
            if self.current_char()? == '"' {
                let string = self.handle_string();

                // fix the position from the advances.
                self.position.1 -= 2;

                tokens.push(Token {
                    lexeme: string,
                    span: Span::new(self.base_pointer, self.pointer),
                    position: self.position,
                });

                // update the position to after string.
                self.position.1 += self.pointer - self.base_pointer;

                continue;
            }

            // if we encounter a number then we handle it as a number but this doesn't account for
            // numbers that start like this: `.100`.
            if self.current_char()?.is_numeric() {
                // this determines if this takes extra chars, for backwards compat.
                let mut extra_chars = 0;

                // if this is a hexadecimal number.
                let number = if self.accept("0x") {
                    extra_chars = 2;
                    self.handle_number(16)
                // if this is a binary number.
                } else if self.accept("0b") {
                    extra_chars = 2;
                    self.handle_number(2)
                // if this is denary number.
                } else {
                    self.handle_number(10)
                };

                tokens.push(Token {
                    lexeme: number,
                    span: Span::new(self.base_pointer, self.pointer),
                    position: self.position,
                });

                // update the position to after number.
                self.position.1 += self.pointer - self.base_pointer + extra_chars;

                continue;
            }

            if self.current_char()?.is_alphabetic() {
                // the raw lexeme identifier.
                let identifier = self.handle_identifier()?;

                tokens.push(Token {
                    lexeme: identifier,
                    span: Span::new(self.base_pointer, self.pointer),
                    position: self.position,
                });

                // update the position to after the identifier.
                self.position = (
                    self.position.0,
                    self.position.1 + (self.pointer - self.base_pointer) + 1,
                );

                // increase the pointer and move onto the next.
                self.pointer += 1;

                continue;
            }

            if self.accept_multicharacter_token(">=", Lexeme::GREATER_EQUAL, &mut tokens) {
                continue;
            }

            if self.accept_multicharacter_token("<=", Lexeme::LESS_EQUAL, &mut tokens) {
                continue;
            }

            if self.accept_multicharacter_token("==", Lexeme::EQ, &mut tokens) {
                continue;
            }

            if self.accept_multicharacter_token("<>", Lexeme::NEQ, &mut tokens) {
                continue;
            }

            let lexeme = match self.current_char()? {
                '+' => Lexeme::ADD,
                '-' => Lexeme::SUBTRACT,
                '*' => Lexeme::MULTIPLY,
                '/' => Lexeme::DIVIDE,
                '(' => Lexeme::LEFT_PAREN,
                ')' => Lexeme::RIGHT_PAREN,
                '^' => Lexeme::XOR,
                '.' => Lexeme::DOT,
                ',' => Lexeme::COMMA,
                ';' => Lexeme::SEMICOLON,
                ':' => Lexeme::COLON,
                ']' => Lexeme::RIGHT_BRACKET,
                '[' => Lexeme::LEFT_BRACKET,
                '{' => Lexeme::LEFT_BRACE,
                '}' => Lexeme::RIGHT_BRACE,
                '%' => Lexeme::MODULO,
                '>' => Lexeme::GREATER_THAN,
                '<' => Lexeme::LESS_THAN,
                '=' => Lexeme::ASSIGN,
                _ => Lexeme::UNDEFINED,
            };

            tokens.push(Token {
                lexeme: lexeme.clone(),
                span: Span::new(self.pointer, self.pointer),
                position: self.position,
            });

            if lexeme == Lexeme::UNDEFINED {
                self.errored = true;
                error!(
                    "invalid token (line: {}, col: {}): {}",
                    self.position.0,
                    self.position.1,
                    self.current_char()?
                );
            }

            self.single_advance();
        }

        if self.errored {
            return Err(LexerError::LexingFailed);
        }

        Ok(tokens)
    }
}
