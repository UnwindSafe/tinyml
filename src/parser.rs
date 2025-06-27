use log::error;

use crate::lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    pointer: usize,
    errored: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pointer: 0,
            errored: false,
        }
    }

    /// Get the current token that is being pointed to.
    fn current_token(&self) -> Option<Token> {
        self.tokens.get(self.pointer).cloned()
    }

    /// Increment the pointer then return the new current token.
    fn advance(&mut self) -> Option<Token> {
        self.pointer += 1;
        self.current_token()
    }

    /// Peek, `n`, tokens ahead. By default it peeks a single token ahead.
    fn peek(&self, n: Option<usize>) -> Option<Token> {
        self.tokens.get(self.pointer + n.unwrap_or(1)).cloned()
    }

    /// Check to see if the next token matches the provided token.
    fn is_match(&self, token: Token) -> bool {
        if let Some(t) = self.peek(None) {
            if t == token {
                return true;
            }
        }
        false
    }

    // If matched consumes the target token.
    fn accept(&mut self, token: Token) -> bool {
        if self.is_match(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    // Must match and consume this token otherwise error.
    fn expect(&mut self, token: Token) {
        if !self.accept(token.clone()) {
            self.errored = true;
            error!(
                "expected symbol: {:?}, found {:?}.",
                token,
                self.current_token(),
            );
        }
    }
}
