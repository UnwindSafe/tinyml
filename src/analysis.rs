use crate::lexer::{Lexeme, Token};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum SemAnalysisError {
    #[error("Undefined variable referenced.")]
    UndefinedVariable,
    #[error("Invalid token type, expected identifier.")]
    NonIdentifier,
}

type Result<T> = std::result::Result<T, SemAnalysisError>;

#[derive(Clone)]
enum Type {
    Int,
    Float,
    String,
    Bool,
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

struct TypePass {
    scope: Rc<RefCell<Scope>>,
}
