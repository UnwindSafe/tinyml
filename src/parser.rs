use crate::{
    analysis,
    lexer::{Lexeme, LexemeKind, Span, Token},
};
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

pub trait AstVisitor {
    type Result;

    // methods for declarations.
    fn visit_decl_val(&mut self, ident: &Token, expr: &mut Expr) -> Self::Result;
    fn visit_decl_function(
        &mut self,
        name: &Token,
        arguments: &[Token],
        body: &mut Expr,
    ) -> Self::Result;

    // methods for expressions.
    fn visit_expr_let(&mut self, ident: &Token, eq: &mut Expr, in_: &mut Expr) -> Self::Result;
    fn visit_expr_binary_op(
        &mut self,
        lhs: &mut Expr,
        symbol: &Token,
        rhs: &mut Expr,
    ) -> Self::Result;
    fn visit_expr_unary_op(&mut self, symbol: &Token, rhs: &mut Expr) -> Self::Result;
    fn visit_expr_if(
        &mut self,
        predicate: &mut Expr,
        then: &mut Expr,
        else_: &mut Expr,
    ) -> Self::Result;
    fn visit_expr_literal(&mut self, token: &Token) -> Self::Result;
}

#[derive(Debug)]
pub enum DeclKind {
    Val {
        ident: Token,
        expr: Box<Expr>,
    },
    Function {
        name: Token,
        arguments: Vec<Token>,
        body: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct Decl {
    kind: DeclKind,
    ty: Option<analysis::Type>,
}

impl Decl {
    fn new(kind: DeclKind) -> Self {
        Self { kind, ty: None }
    }

    pub fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> V::Result {
        match &mut self.kind {
            DeclKind::Val { ident, expr } => visitor.visit_decl_val(ident, expr),
            DeclKind::Function {
                name,
                arguments,
                body,
            } => visitor.visit_decl_function(name, arguments, body),
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
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
    If {
        predicate: Box<Expr>,
        then_: Box<Expr>,
        else_: Box<Expr>,
    },
    Literal(Token),
}

#[derive(Debug)]
pub struct Expr {
    kind: ExprKind,
    ty: Option<analysis::Type>,
}

impl Expr {
    fn new(kind: ExprKind) -> Self {
        Self { kind, ty: None }
    }

    pub fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> V::Result {
        match &mut self.kind {
            ExprKind::If {
                predicate,
                then_,
                else_,
            } => visitor.visit_expr_if(predicate, then_, else_),
            ExprKind::Let { ident, eq_, in_ } => visitor.visit_expr_let(ident, eq_, in_),
            ExprKind::BinaryOp { lhs, symbol, rhs } => {
                visitor.visit_expr_binary_op(lhs, symbol, rhs)
            }
            ExprKind::UnaryOp { symbol, rhs } => visitor.visit_expr_unary_op(symbol, rhs),
            ExprKind::Literal(token) => visitor.visit_expr_literal(token),
        }
    }
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
            expr = Expr::new(ExprKind::BinaryOp {
                lhs: Box::new(expr),
                symbol: token,
                rhs: Box::new(self.comparison()?),
            });
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
            expr = Expr::new(ExprKind::BinaryOp {
                lhs: Box::new(expr),
                symbol: token,
                rhs: Box::new(self.term()?),
            });
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while let Some(token) = self.accept_lexemes([LexemeKind::ADD, LexemeKind::SUBTRACT]) {
            expr = Expr::new(ExprKind::BinaryOp {
                lhs: Box::new(expr),
                symbol: token,
                rhs: Box::new(self.factor()?),
            });
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while let Some(token) = self.accept_lexemes([LexemeKind::MULTIPLY, LexemeKind::DIVIDE]) {
            expr = Expr::new(ExprKind::BinaryOp {
                lhs: Box::new(expr),
                symbol: token,
                rhs: Box::new(self.unary()?),
            });
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        while let Some(token) = self.accept_lexemes([LexemeKind::EXCLAMATION, LexemeKind::TILDE]) {
            return Ok(Expr::new(ExprKind::UnaryOp {
                symbol: token,
                rhs: Box::new(self.unary()?),
            }));
        }

        Ok(self.primary()?)
    }

    fn primary(&mut self) -> Result<Expr> {
        if self.accept(LexemeKind::LEFT_PAREN) {
            // parse the expression inside the parentheses.
            let expr = self.expr()?;
            self.expect(LexemeKind::RIGHT_PAREN)?;
            return Ok(expr);
        }

        if self.accept(LexemeKind::IF) {
            let predicate = self.expr()?;
            self.expect(LexemeKind::THEN)?;
            let then_ = self.expr()?;
            self.expect(LexemeKind::ELSE)?;
            let else_ = self.expr()?;

            return Ok(Expr::new(ExprKind::If {
                predicate: Box::new(predicate),
                then_: Box::new(then_),
                else_: Box::new(else_),
            }));
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
            return Ok(Expr::new(ExprKind::Literal(token)));
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

    pub fn decl(&mut self) -> Result<Option<Decl>> {
        if self.accept(LexemeKind::VAL) {
            let ident = self.expect(LexemeKind::IDENTIFIER)?;
            self.expect(LexemeKind::ASSIGN)?;
            let expr = self.expr()?;

            return Ok(Some(Decl::new(DeclKind::Val {
                ident,
                expr: Box::new(expr),
            })));
        }

        if self.accept(LexemeKind::FUNCTION) {
            let ident = self.expect(LexemeKind::IDENTIFIER)?;

            // a list of arguments that we have for the function.
            let mut arguments: Vec<Token> = Vec::new();

            if self.accept(LexemeKind::LEFT_PAREN) {
                while let Some(token) = self.accept_lexemes([LexemeKind::IDENTIFIER]) {
                    arguments.push(token);
                    // optionally consume a comma if the user entered one.
                    self.accept(LexemeKind::COMMA);
                }
                self.expect(LexemeKind::RIGHT_PAREN)?;
            }

            self.expect(LexemeKind::ASSIGN)?;

            let body = self.expr()?;

            return Ok(Some(Decl::new(DeclKind::Function {
                name: ident,
                arguments,
                body: Box::new(body),
            })));
        }

        Ok(None)
    }

    pub fn parse(&mut self) -> Result<Vec<Decl>> {
        // a list of all the declarations.
        let mut decls: Vec<Decl> = Vec::new();

        while let Some(decl) = self.decl()? {
            decls.push(decl);
        }

        Ok(decls)
    }
}
