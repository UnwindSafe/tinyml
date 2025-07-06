use crate::{
    lexer::{Lexeme, Token},
    parser::{AstVisitor, Decl, Expr},
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use log::error;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SemAnalysisError {
    #[error("Undefined variable referenced.")]
    UndefinedVariable,
    #[error("Invalid token type, expected identifier.")]
    NonIdentifier,
}

type Result<T> = std::result::Result<T, SemAnalysisError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Void,
    /// 0: list of argument types, 1: function return type.
    Function(Vec<Type>, Box<Type>),
}

#[derive(Clone)]
struct Symbol {
    ty: Type,
    identifier: Token,
}

struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    symbols: HashMap<String, Symbol>,
}

impl Scope {
    fn new(parent: Option<Rc<RefCell<Scope>>>) -> Self {
        Self {
            parent,
            symbols: HashMap::new(),
        }
    }

    /// This will find the Symbol in the scope tree if it is defined.
    fn resolve(&self, identifier: Token) -> Result<Symbol> {
        // get the identifier string from the token.
        let ident = match identifier.lexeme {
            Lexeme::IDENTIFIER(ref s) => Ok(s),
            _ => Err(SemAnalysisError::NonIdentifier),
        }?;

        if let Some(s) = self.symbols.get(ident) {
            return Ok(s.clone());
        }

        // call this function on the parent scope.
        if let Some(p) = &self.parent {
            return p.borrow_mut().resolve(identifier);
        }

        Err(SemAnalysisError::UndefinedVariable)
    }

    /// Define a symbol for the current scope.
    fn define(&mut self, symbol: Symbol) -> Result<()> {
        // get the identifier string from the token.
        let ident = match symbol.identifier.lexeme {
            Lexeme::IDENTIFIER(ref s) => Ok(s),
            _ => Err(SemAnalysisError::NonIdentifier),
        }?;

        self.symbols.insert(ident.to_string(), symbol);

        Ok(())
    }
}

struct Typecheck {
    scope: Rc<RefCell<Scope>>,
}

impl AstVisitor for Typecheck {
    type Result = Type;

    fn visit_decl_val(&mut self, ident: &Token, expr: &mut Expr) -> Self::Result {
        expr.accept(self)
    }

    fn visit_decl_function(
        &mut self,
        name: &Token,
        arguments: &[Token],
        body: &mut Expr,
    ) -> Self::Result {
        todo!()
    }

    fn visit_expr_let(&mut self, ident: &Token, eq: &mut Expr, in_: &mut Expr) -> Self::Result {
        todo!()
    }

    fn visit_expr_binary_op(
        &mut self,
        lhs: &mut Expr,
        symbol: &Token,
        rhs: &mut Expr,
    ) -> Self::Result {
        let _lhs = lhs.accept(self);
        let _rhs = rhs.accept(self);

        if _rhs != _lhs {
            error!("type mismatch: {:?} != {:?}", _lhs, _rhs);
        }
        Type::Void
    }

    fn visit_expr_unary_op(&mut self, symbol: &Token, rhs: &mut Expr) -> Self::Result {
        todo!()
    }

    fn visit_expr_if(
        &mut self,
        predicate: &mut Expr,
        then: &mut Expr,
        else_: &mut Expr,
    ) -> Self::Result {
        todo!()
    }

    fn visit_expr_literal(&mut self, token: &Token) -> Self::Result {
        match &token.lexeme {
            Lexeme::NUMBER(_) => Type::Float,
            Lexeme::STRING(_) => Type::String,
            Lexeme::IDENTIFIER(s) => Type::Void,
            Lexeme::TRUE | Lexeme::FALSE => Type::Bool,
            _ => Type::Void,
        }
    }
}

pub fn run_passes(ast: &mut Vec<Decl>) {
    // create a global scope.
    let global = Rc::new(RefCell::new(Scope::new(None)));

    let mut type_check = Typecheck { scope: global };

    for decl in ast.iter_mut() {
        decl.accept(&mut type_check);
    }
}
