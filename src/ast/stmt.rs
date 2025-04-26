use crate::token::Token;

use super::{Expr, StmtVisitor, StmtWalkable};

#[allow(rustdoc::invalid_rust_codeblocks)]
/// A statement or declaration AST node.
///
/// Grammar, using the book's version of BNF:
///
/// ```ignore
/// declaration    → varDecl
///                | statement ;
///
/// statement      → exprStmt
///                | printStmt ;
///
/// exprStmt       → expression ";" ;
/// printStmt      → "print" expression ";" ;
/// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
/// ```
#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    Print(Expr<'a>),
    Var {
        name: Token<'static>,
        initializer: Option<Expr<'static>>,
    },
}

impl Stmt<'_> {
    pub fn into_owned(self) -> Stmt<'static> {
        match self {
            Stmt::Expression(e) => Stmt::Expression(e.into_owned()),
            Stmt::Print(e) => Stmt::Print(e.into_owned()),
            Stmt::Var { name, initializer } => Stmt::Var { name, initializer },
        }
    }
}

impl<R> StmtWalkable<R> for Stmt<'_> {
    fn walk_stmt<'a, V>(&'a self, visitor: &mut V) -> R
    where
        V: StmtVisitor<Ret<'a> = R>,
    {
        match self {
            Stmt::Expression(..) => visitor.visit_expression_stmt(self),
            Stmt::Print(..) => visitor.visit_print_stmt(self),
            Stmt::Var { .. } => visitor.visit_var_stmt(self),
        }
    }
}
