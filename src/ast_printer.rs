use crate::expr::{self, Expr, Walkable};

/// Converts an expression tree to a string using Lisp-like groupings.
///
/// Use this struct's [`AstPrinter::walk_ast()`] implementation to actually
/// walk the expression tree and convert it to a string.
pub struct AstPrinter<'a>(pub &'a Expr);

impl<'a> From<&'a Expr> for AstPrinter<'a> {
    fn from(expr: &'a Expr) -> Self {
        AstPrinter(expr)
    }
}

impl AstPrinter<'_> {
    /// Walk the expression tree and convert it to a string.
    pub fn walk_ast(&mut self) -> String {
        self.0.walk(self)
    }

    fn parenthesize<I, E>(&mut self, name: &str, exprs: I) -> String
    where
        I: IntoIterator<Item = E>,
        E: AsRef<Expr>,
    {
        let mut out = '('.to_string();
        out.push_str(name);

        for expr in exprs.into_iter() {
            out.push(' ');
            out.push_str(&expr.as_ref().walk(self));
        }

        out.push(')');

        out
    }
}

impl expr::Visitor<String> for AstPrinter<'_> {
    fn visit_grouping_expr(&mut self, grouping_expr: &Expr) -> String {
        let Expr::Grouping(expression) = grouping_expr else {
            unreachable!("should always be a grouping expr");
        };

        self.parenthesize("group", [expression])
    }

    fn visit_literal_expr(&mut self, literal_expr: &Expr) -> String {
        let Expr::Literal(value) = literal_expr else {
            unreachable!("should always be a literal expr");
        };
        if let Some(lit) = value {
            lit.to_string()
        } else {
            "nil".to_string()
        }
    }

    fn visit_unary_expr(&mut self, unary_expr: &Expr) -> String {
        let Expr::Unary { operator, right } = unary_expr else {
            unreachable!("should always be an unary expr");
        };

        self.parenthesize(operator.lexeme(), [right])
    }

    fn visit_binary_expr(&mut self, binary_expr: &Expr) -> String {
        let Expr::Binary {
            left,
            operator,
            right,
        } = binary_expr
        else {
            unreachable!("should always be a binary expr");
        };

        self.parenthesize(operator.lexeme(), [left, right])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::rc::Rc;
    
    use crate::token::Token;
    use crate::token_type::TokenType;

    #[test]
    fn ast_print_basic_nested_expr() {
        let expression = Expr::Binary {
            left: Box::new(Expr::Unary {
                operator: Token::new(TokenType::Minus, "-", None, 1),
                right: Box::new(Expr::Literal(Some(Rc::new(123.0)))),
            }),
            operator: Token::new(TokenType::Star, "*", None, 1),
            right: Box::new(Expr::Grouping(Box::new(Expr::Literal(Some(Rc::new(
                45.67,
            )))))),
        };

        assert_eq!(
            AstPrinter::from(&expression).walk_ast(),
            "(* (- 123) (group 45.67))".to_string()
        );
    }
}
