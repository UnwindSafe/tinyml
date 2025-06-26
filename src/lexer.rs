use std::collections::HashMap;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Lexer pointer is out of bounds.")]
    OutOfBounds,
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

    fn handle_number(&mut self) -> Lexeme {
        println!("number");
        unimplemented!()
    }

    fn handle_string(&mut self) -> Lexeme {
        println!("string");
        unimplemented!()
    }

    fn handle_comment(&mut self) {
        println!("comment");
        unimplemented!()
    }

    fn handle_identifier(&mut self) -> Result<Lexeme> {
        // set the base to the start of the identifier.
        self.base_pointer = self.pointer;

        // keep consuming until we're done with the identifier.
        while self.peek(None).is_alphanumeric() || self.peek(None) == '_' {
            self.pointer += 1;
        }

        // this is the complete identifier.
        let identifier = &self.text[self.base_pointer..=self.pointer];

        return Ok(Lexeme::IDENTIFIER(identifier.to_owned()));
    }

    pub fn scan(&mut self) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();

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

        // while we are not at the end of the file.
        while self.pointer < self.text.len() as _ {
            // disregard whitespace.
            if self.current_char()?.is_whitespace() {
                self.single_advance();
                continue;
            }

            // update the state of the lexer if new line.
            if self.current_char()? == '\n' {
                // increase the line and reset the column.
                self.position = (self.position.0 + 1, 1);
                continue;
            }

            if self.accept("(*") {
                self.handle_comment();
                continue;
            }

            // handle a string when we find an opening.
            if self.current_char()? == '"' {
                self.handle_string();
                continue;
            }

            // if we encounter a number then we handle it as a number but this doesn't account for
            // hexadecimal numbers and numbers that start like this: `.100`.
            if self.current_char()?.is_numeric() {
                self.handle_number();
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
                _ => Lexeme::UNDEFINED,
            };

            self.single_advance();
        }

        Ok(tokens)
    }
}
