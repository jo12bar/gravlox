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
pub enum Expr<'a> {
    Grouping(Box<Expr<'static>>),
    Literal(Option<Literal<'a>>),
    Unary {
        operator: Token<'a>,
        right: Box<Expr<'static>>,
    },
    Binary {
        left: Box<Expr<'static>>,
        operator: Token<'a>,
        right: Box<Expr<'static>>,
    },
}

impl Expr<'_> {
    pub fn into_owned(self) -> Expr<'static> {
        match self {
            Expr::Grouping(g) => Expr::Grouping(Box::new(g.into_owned())),
            Expr::Literal(l) => Expr::Literal(l.map(|l| l.into_owned())),
            Expr::Unary { operator, right } => Expr::Unary {
                operator: operator.into_owned(),
                right,
            },
            Expr::Binary {
                left,
                operator,
                right,
            } => Expr::Binary {
                left,
                operator: operator.into_owned(),
                right,
            },
        }
    }
}

/// A trait allowing something to visit a tree of [expressions][Expr].
pub trait Visitor {
    /// The return type of all `visit_x()` functions defined on this trait.
    /// It's allowed to borrow from the [Expr]'s passed to each `visit_x()`.
    type Ret<'a>;

    /// Visit a [`Expr::Grouping`].
    fn visit_grouping_expr<'a, 'r: 'a>(&mut self, grouping_expr: &'r Expr<'a>) -> Self::Ret<'a>;
    /// Visit a [`Expr::Literal`].
    fn visit_literal_expr<'a, 'r: 'a>(&mut self, literal_expr: &'r Expr<'a>) -> Self::Ret<'a>;
    /// Visit a [`Expr::Unary`].
    fn visit_unary_expr<'a, 'r: 'a>(&mut self, unary_expr: &'r Expr<'a>) -> Self::Ret<'a>;
    /// Visit a [`Expr::Binary`].
    fn visit_binary_expr<'a, 'r: 'a>(&mut self, binary_expr: &'r Expr<'a>) -> Self::Ret<'a>;
}

/// Something that can be iterably visited by a [`Visitor`].
///
/// The generic type `R` is the return type of all `visit_x()` functions
/// defined on the passed-in [`Visitor`] implementer, and is the result of
/// calling [`Walkable::walk()`].
pub trait Walkable<R> {
    fn walk<'a, V>(&'a self, visitor: &mut V) -> R
    where
        V: Visitor<Ret<'a> = R>;
}

impl<R> Walkable<R> for Expr<'_> {
    fn walk<'a, V>(&'a self, visitor: &mut V) -> R
    where
        V: Visitor<Ret<'a> = R>,
    {
        match self {
            Expr::Grouping(..) => visitor.visit_grouping_expr(self),
            Expr::Literal(..) => visitor.visit_literal_expr(self),
            Expr::Unary { .. } => visitor.visit_unary_expr(self),
            Expr::Binary { .. } => visitor.visit_binary_expr(self),
        }
    }
}
