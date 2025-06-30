use log::error;

use crate::lexer::{Lexeme, LexemeKind, Token};

#[derive(Debug)]
pub enum Expr {
    Let {
        ident: Token,
        eq_expr: Box<Expr>,
        in_expr: Box<Expr>,
    },
    Literal(Token),
}

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
    fn peek(&self, n: Option<isize>) -> Option<Token> {
        self.tokens
            .get((self.pointer as isize + n.unwrap_or(1)) as usize)
            .cloned()
    }

    /// Check to see if the next token matches the provided token.
    fn is_match(&self, lexeme: LexemeKind) -> bool {
        if let Some(t) = self.peek(None) {
            if t.lexeme.kind() == lexeme {
                return true;
            }
        }
        false
    }

    // If matched consumes the target token.
    fn accept(&mut self, lexeme: LexemeKind) -> bool {
        if self.is_match(lexeme) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Like `accept` but matches multiple tokens, returning the lexme it matched.
    fn accept_lexemes(&mut self, lexemes: impl Iterator<Item = LexemeKind>) -> Option<LexemeKind> {
        for lexeme in lexemes {
            if self.accept(lexeme.clone()) {
                return Some(lexeme);
            }
        }
        None
    }

    /// Gets the token before the current token.
    fn previous(&self) -> Option<Token> {
        self.peek(Some(-1))
    }

    // Must match and consume this token otherwise error.
    fn expect(&mut self, lexeme: LexemeKind) -> Result<Token, ()> {
        if !self.accept(lexeme) {
            self.errored = true;
            error!(
                "expected symbol: {:?}, found {:?}.",
                lexeme,
                self.current_token(),
            );
            return Err(());
        }

        Ok(self.previous().unwrap())
    }

    fn expr(&mut self) -> Result<Expr, ()> {
        // "let" <ident> "=" <expr> "in" <expr> "end".
        if self.accept(LexemeKind::LET) {
            let ident = self.expect(LexemeKind::IDENTIFIER).unwrap();
            self.expect(LexemeKind::EQ)?;
            let eq_expr = self.expr()?;
            self.expect(LexemeKind::IN)?;
            let in_expr = self.expr()?;
            self.expect(LexemeKind::END)?;

            return Ok(Expr::Let {
                ident,
                eq_expr: Box::new(eq_expr),
                in_expr: Box::new(in_expr),
            });
        }

        Err(())
    }

    pub fn parse(&mut self) -> Expr {
        self.expr().unwrap()
    }
}
