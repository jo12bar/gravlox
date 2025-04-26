use super::{Expr, StmtVisitor, StmtWalkable};

#[allow(rustdoc::invalid_rust_codeblocks)]
/// A statement AST node.
///
/// Grammar, using the book's version of BNF:
///
/// ```ignore
/// statement      → exprStmt
///                | printStmt ;
///
/// exprStmt       → expression ";" ;
/// printStmt      → "print" expression ";" ;
/// ```
#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    Print(Expr<'a>),
}

impl Stmt<'_> {
    pub fn into_owned(self) -> Stmt<'static> {
        match self {
            Stmt::Expression(e) => Stmt::Expression(e.into_owned()),
            Stmt::Print(e) => Stmt::Print(e.into_owned()),
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
        }
    }
}
