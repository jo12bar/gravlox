use crate::{literal::Literal, token::Token};

use super::{ExprVisitor, ExprWalkable};

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
/// primary        → "true" | "false" | "nil"
///                | NUMBER | STRING
///                | "(" expression ")"
///                | IDENTIFIER ;
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
    Var {
        name: Token<'a>,
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
            Expr::Var { name } => Expr::Var {
                name: name.into_owned(),
            },
        }
    }
}

impl<R> ExprWalkable<R> for Expr<'_> {
    fn walk_expr<'a, V>(&'a self, visitor: &mut V) -> R
    where
        V: ExprVisitor<Ret<'a> = R>,
    {
        match self {
            Expr::Grouping(..) => visitor.visit_grouping_expr(self),
            Expr::Literal(..) => visitor.visit_literal_expr(self),
            Expr::Unary { .. } => visitor.visit_unary_expr(self),
            Expr::Binary { .. } => visitor.visit_binary_expr(self),
            Expr::Var { .. } => visitor.visit_var_expr(self),
        }
    }
}
