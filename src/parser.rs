use crate::lexer::{Lexeme, LexemeKind, Span, Token};
use log::error;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Error while parsing expression.")]
    ExpressionError,
    #[error("Parser pointer is out of bounds.")]
    OutOfBounds,
}

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub enum Expr {
    Let {
        ident: Token,
        eq_: Box<Expr>,
        in_: Box<Expr>,
    },
    BinaryOp {
        lhs: Box<Expr>,
        symbol: Token,
        rhs: Box<Expr>,
    },
    UnaryOp {
        symbol: Token,
        rhs: Box<Expr>,
    },
    Literal(Token),
}

pub struct Parser {
    tokens: Vec<Token>,
    pointer: usize,
    errored: bool,
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Self {
        let last_token = tokens.last().unwrap();

        // append EOF token to the end of token list.
        tokens.push(Token {
            lexeme: Lexeme::EOF,
            span: last_token.span.clone(),
            position: last_token.position,
        });

        Self {
            tokens,
            pointer: 0,
            errored: false,
        }
    }

    /// Get the current token that is being pointed to.
    fn current_token(&self) -> Result<Token> {
        self.tokens
            .get(self.pointer)
            .cloned()
            .ok_or(ParserError::OutOfBounds)
    }

    /// Increment the pointer then return the new current token.
    fn advance(&mut self) -> Result<Token> {
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
        if let Some(t) = self.peek(Some(0)) {
            if t.lexeme.kind() == lexeme {
                return true;
            }
        }
        false
    }

    // If matched consumes the target token.
    fn accept(&mut self, lexeme: LexemeKind) -> bool {
        if self.is_match(lexeme) {
            let _ = self.advance();
            true
        } else {
            false
        }
    }

    /// Like `accept` but matches multiple tokens, returning the lexme it matched.
    fn accept_lexemes(&mut self, lexemes: impl IntoIterator<Item = LexemeKind>) -> Option<Token> {
        for lexeme in lexemes {
            if self.accept(lexeme.clone()) {
                return self.previous().ok();
            }
        }
        None
    }

    /// Gets the token before the current token.
    fn previous(&self) -> Result<Token> {
        self.peek(Some(-1)).ok_or(ParserError::OutOfBounds)
    }

    // Must match and consume this token otherwise error.
    fn expect(&mut self, lexeme: LexemeKind) -> Result<Token> {
        if !self.accept(lexeme) {
            self.errored = true;
            error!(
                "expected symbol: {:?}, found: {:?} (line {}:{})",
                lexeme,
                self.current_token()?.lexeme,
                self.current_token()?.position.0,
                self.current_token()?.position.1,
            );
            return Err(ParserError::ExpressionError);
        }

        Ok(self.previous()?)
    }

    // Consume tokens until parser is in a predictable state.
    fn synchronize(&mut self) {
        while !self.accept(LexemeKind::SEMICOLON) {
            let _ = self.advance();
        }
    }

    fn expr(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while let Some(token) = self.accept_lexemes([LexemeKind::EQ, LexemeKind::NEQ]) {
            expr = Expr::BinaryOp {
                lhs: Box::new(expr),
                symbol: token,
                rhs: Box::new(self.comparison()?),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;

        while let Some(token) = self.accept_lexemes([
            LexemeKind::GREATER_THAN,
            LexemeKind::GREATER_EQUAL,
            LexemeKind::LESS_THAN,
            LexemeKind::LESS_EQUAL,
        ]) {
            expr = Expr::BinaryOp {
                lhs: Box::new(expr),
                symbol: token,
                rhs: Box::new(self.term()?),
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while let Some(token) = self.accept_lexemes([LexemeKind::ADD, LexemeKind::SUBTRACT]) {
            expr = Expr::BinaryOp {
                lhs: Box::new(expr),
                symbol: token,
                rhs: Box::new(self.factor()?),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while let Some(token) = self.accept_lexemes([LexemeKind::MULTIPLY, LexemeKind::DIVIDE]) {
            expr = Expr::BinaryOp {
                lhs: Box::new(expr),
                symbol: token,
                rhs: Box::new(self.unary()?),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        while let Some(token) = self.accept_lexemes([LexemeKind::EXCLAMATION, LexemeKind::TILDE]) {
            return Ok(Expr::UnaryOp {
                symbol: token,
                rhs: Box::new(self.unary()?),
            });
        }

        Ok(self.primary()?)
    }

    fn primary(&mut self) -> Result<Expr> {
        // "let" <ident> "=" <expr> "in" <expr> "end".
        if self.accept(LexemeKind::LET) {
            // get the identifier for the let epression.
            let ident = self.expect(LexemeKind::IDENTIFIER)?;

            self.expect(LexemeKind::ASSIGN)?;

            // get the expression after the assignment operator.
            let eq_expr = self.expr()?;

            self.expect(LexemeKind::IN)?;

            // get the expression after the in operator.
            let in_expr = self.expr()?;

            self.expect(LexemeKind::END)?;

            return Ok(Expr::Let {
                ident,
                eq_: Box::new(eq_expr),
                in_: Box::new(in_expr),
            });
        }

        // a list of accepted literals.
        let literals = [
            LexemeKind::NUMBER,
            LexemeKind::STRING,
            LexemeKind::TRUE,
            LexemeKind::FALSE,
            LexemeKind::IDENTIFIER,
        ];

        if let Some(token) = self.accept_lexemes(literals) {
            return Ok(Expr::Literal(token));
        }

        error!(
            "expected: LET, {:?}, found: {:?} (line {}:{})",
            literals,
            self.current_token()?.lexeme,
            self.current_token()?.position.0,
            self.current_token()?.position.1
        );

        Err(ParserError::ExpressionError)
    }

    pub fn parse(&mut self) -> Result<Expr> {
        Ok(self.expr()?)
    }
}
