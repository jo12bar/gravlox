//! AST defining a program.
//!
//! A program is just a list of declarations terminated by a special "end of file"
//! token. Using the book's version of BNF:
//!
//! ```text
//! program        → declaration* EOF ;
//! ```
//!

pub mod expr;
pub mod stmt;

pub use expr::Expr;
pub use stmt::Stmt;

/// A trait allowing something to visit a tree of [expressions][Expr].
pub trait ExprVisitor {
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
    /// Visit a [`Expr::Var`].
    fn visit_var_expr<'a, 'r: 'a>(&mut self, var_expr: &'r Expr<'a>) -> Self::Ret<'a>;
}

/// A trait allowing something to visit a tree of [statements][Stmt].
pub trait StmtVisitor {
    /// The return type of all `visit_x()` functions defined on this trait.
    /// It's allowed to borrow from the [Stmt]'s passed to each `visit_x()`.
    type Ret<'a>;

    /// Visit a [`Stmt::Expression`].
    fn visit_expression_stmt<'a, 'r: 'a>(&mut self, expression_stmt: &'r Stmt<'a>)
    -> Self::Ret<'a>;
    /// Visit a [`Stmt::Print`].
    fn visit_print_stmt<'a, 'r: 'a>(&mut self, print_stmt: &'r Stmt<'a>) -> Self::Ret<'a>;
    /// Visit a [`Stmt::Var`].
    fn visit_var_stmt<'a, 'r: 'a>(&mut self, var_stmt: &'r Stmt<'a>) -> Self::Ret<'a>;
}

/// Something that can be iterably visited by an [`ExprVisitor`].
///
/// The generic type `R` is the return type of all `visit_x()` functions
/// defined on the passed-in [`ExprVisitor`] implementer, and is the result of
/// calling [`ExprWalkable::walk()`].
pub trait ExprWalkable<R> {
    fn walk_expr<'a, V>(&'a self, visitor: &mut V) -> R
    where
        V: ExprVisitor<Ret<'a> = R>;
}

/// Something that can be iterably visited by n [`StmtVisitor`].
///
/// The generic type `R` is the return type of all `visit_x()` functions
/// defined on the passed-in [`StmtVisitor`] implementer, and is the result of
/// calling [`StmtWalkable::walk()`].
pub trait StmtWalkable<R> {
    fn walk_stmt<'a, V>(&'a self, visitor: &mut V) -> R
    where
        V: StmtVisitor<Ret<'a> = R>;
}
