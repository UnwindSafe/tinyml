use std::collections::HashMap;

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
}

/// This is the span of the lexeme in the text.
#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    start: u32,
    end: u32,
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

    fn current_char(&self) -> Option<char> {
        self.text.chars().nth(self.pointer)
    }

    fn handle_number(&mut self) {}

    fn handle_string(&mut self) {}

    fn handle_comment(&mut self) {}

    fn handle_identifier(&mut self) {}

    pub fn scan(&mut self) -> Vec<Token> {
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
            if self.current_char().unwrap().is_whitespace() {
                continue;
            }
        }

        tokens
    }
}
