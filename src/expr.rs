use std::rc::Rc;

use crate::{literal::Literal, token::Token};

#[allow(rustdoc::invalid_rust_codeblocks)]
/// An expression AST node.
///
/// Grammar, using the book's version of BNF:
///
/// ```ignore
/// expression     → comma ;
/// comma          → equality ( "," equality )* ;
/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term           → factor ( ( "-" | "+" ) factor )* ;
/// factor         → unary ( ( "/" | "*" ) unary )* ;
/// unary          → ( "!" | "-" ) unary
///                | primary ;
/// primary        → NUMBER | STRING | "true" | "false" | "nil"
///                | "(" expression ")" ;
/// ```
#[derive(Debug, Clone)]
pub enum Expr {
    Grouping(Box<Expr>),
    Literal(Option<Rc<dyn Literal>>),
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
}

/// A trait allowing something to visit a tree of [expressions][Expr].
///
/// The generic type `R` is the return type of all `visit_x()` functions
/// defined on this trait.
pub trait Visitor<R> {
    /// Visit a [`Expr::Grouping`].
    fn visit_grouping_expr(&mut self, grouping_expr: &Expr) -> R;
    /// Visit a [`Expr::Literal`].
    fn visit_literal_expr(&mut self, literal_expr: &Expr) -> R;
    /// Visit a [`Expr::Unary`].
    fn visit_unary_expr(&mut self, unary_expr: &Expr) -> R;
    /// Visit a [`Expr::Binary`].
    fn visit_binary_expr(&mut self, binary_expr: &Expr) -> R;
}

/// Something that can be iterably visited by a [`Visitor`].
///
/// The generic type `R` is the return type of all `visit_x()` functions
/// defined on the passed-in [`Visitor`] implementer, and is the result of
/// calling [`Walkable::walk()`].
pub trait Walkable<R> {
    fn walk<V>(&self, visitor: &mut V) -> R
    where
        V: Visitor<R>;
}

impl<R> Walkable<R> for Expr {
    fn walk<V>(&self, visitor: &mut V) -> R
    where
        V: Visitor<R>,
    {
        match self {
            Expr::Grouping(..) => visitor.visit_grouping_expr(self),
            Expr::Literal(..) => visitor.visit_literal_expr(self),
            Expr::Unary { .. } => visitor.visit_unary_expr(self),
            Expr::Binary { .. } => visitor.visit_binary_expr(self),
        }
    }
}
