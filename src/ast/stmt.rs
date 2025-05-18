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
///                | forStmt
///                | ifStmt
///                | printStmt
///                | whileStmt
///                | block ;
///
/// exprStmt       → expression ";" ;
/// forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
///                  expression? ";"
///                  expression? ")" statement ;
/// ifStmt         → "if" "(" expression ")" statement
///                  ( "else" statement )? ;
/// printStmt      → "print" expression ";" ;
/// whileStmt      → "while" "(" expression ")" statement ;
/// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
/// block          → "{" declaration* "}" ;
/// ```
#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    If {
        condition: Expr<'static>,
        then_branch: Box<Stmt<'static>>,
        else_branch: Option<Box<Stmt<'static>>>,
    },
    Print(Expr<'a>),
    While {
        condition: Expr<'a>,
        body: Box<Stmt<'static>>,
    },
    Var {
        name: Token<'static>,
        initializer: Option<Expr<'static>>,
    },
    Block {
        statements: Vec<Stmt<'static>>,
    },
}

impl Stmt<'_> {
    pub fn into_owned(self) -> Stmt<'static> {
        match self {
            Stmt::Expression(e) => Stmt::Expression(e.into_owned()),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => Stmt::If {
                condition,
                then_branch,
                else_branch,
            },
            Stmt::Print(e) => Stmt::Print(e.into_owned()),
            Stmt::While { condition, body } => Stmt::While {
                condition: condition.into_owned(),
                body,
            },
            Stmt::Var { name, initializer } => Stmt::Var { name, initializer },
            Stmt::Block { statements } => Stmt::Block { statements },
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
            Stmt::If { .. } => visitor.visit_if_stmt(self),
            Stmt::Print(..) => visitor.visit_print_stmt(self),
            Stmt::While { .. } => visitor.visit_while_stmt(self),
            Stmt::Var { .. } => visitor.visit_var_stmt(self),
            Stmt::Block { .. } => visitor.visit_block_stmt(self),
        }
    }
}
