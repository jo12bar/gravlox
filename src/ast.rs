mod expr;
mod stmt;

pub use expr::Expr;

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
