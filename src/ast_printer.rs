use crate::ast::{self, Expr, ExprWalkable};

/// Converts an expression tree to a string using Lisp-like groupings.
///
/// Use this struct's [`AstPrinter::walk_ast()`] implementation to actually
/// walk the expression tree and convert it to a string.
pub struct AstPrinter<'a, 'expr>(pub &'a Expr<'expr>);

impl<'a, 'expr> From<&'a Expr<'expr>> for AstPrinter<'a, 'expr> {
    fn from(expr: &'a Expr<'expr>) -> Self {
        AstPrinter(expr)
    }
}

impl AstPrinter<'_, '_> {
    /// Walk the expression tree and convert it to a string.
    pub fn walk_ast(&mut self) -> String {
        self.0.walk_expr(self)
    }

    fn parenthesize<'e, I, E>(&mut self, name: &str, exprs: I) -> String
    where
        I: IntoIterator<Item = E>,
        E: AsRef<Expr<'e>>,
    {
        let mut out = '('.to_string();
        out.push_str(name);

        for expr in exprs.into_iter() {
            out.push(' ');
            out.push_str(&expr.as_ref().walk_expr(self));
        }

        out.push(')');

        out
    }
}

impl ast::ExprVisitor for AstPrinter<'_, '_> {
    type Ret<'r> = String;

    fn visit_grouping_expr<'e, 'r: 'e>(&mut self, grouping_expr: &'r Expr<'e>) -> String {
        let Expr::Grouping(expression) = grouping_expr else {
            unreachable!("should always be a grouping expr");
        };

        self.parenthesize("group", [expression])
    }

    fn visit_literal_expr<'e, 'r: 'e>(&mut self, literal_expr: &'r Expr<'e>) -> String {
        let Expr::Literal(value) = literal_expr else {
            unreachable!("should always be a literal expr");
        };
        if let Some(lit) = value {
            match lit {
                crate::literal::Literal::Num(n) => n.to_string(),
                crate::literal::Literal::Bool(b) => b.to_string(),
                crate::literal::Literal::String(s) => format!("\"{s}\""),
            }
        } else {
            "nil".to_string()
        }
    }

    fn visit_unary_expr<'e, 'r: 'e>(&mut self, unary_expr: &'r Expr<'e>) -> String {
        let Expr::Unary { operator, right } = unary_expr else {
            unreachable!("should always be an unary expr");
        };

        self.parenthesize(operator.lexeme(), [right])
    }

    fn visit_binary_expr<'e, 'r: 'e>(&mut self, binary_expr: &'r Expr<'e>) -> String {
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

    fn visit_var_expr<'a, 'r: 'a>(&mut self, var_expr: &'r Expr<'a>) -> Self::Ret<'a> {
        let Expr::Var { name } = var_expr else {
            unreachable!("should always be a var expression");
        };

        name.lexeme().to_string()
    }

    fn visit_assign_expr<'a, 'r: 'a>(&mut self, assign_expr: &'r Expr<'a>) -> Self::Ret<'a> {
        let Expr::Assign { name, value } = assign_expr else {
            unreachable!("should always be an assign expr");
        };

        self.parenthesize(
            "assign",
            [&Box::new(Expr::Var { name: name.clone() }), value],
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::literal::Literal;
    use crate::token::Token;
    use crate::token_type::TokenType;

    #[test]
    fn ast_print_basic_nested_expr() {
        let expression = Expr::Binary {
            left: Box::new(Expr::Unary {
                operator: Token::new(TokenType::Minus, "-", None, 1),
                right: Box::new(Expr::Literal(Some(Literal::Num(123.0)))),
            }),
            operator: Token::new(TokenType::Star, "*", None, 1),
            right: Box::new(Expr::Grouping(Box::new(Expr::Literal(Some(Literal::Num(
                45.67,
            )))))),
        };

        assert_eq!(
            AstPrinter::from(&expression).walk_ast(),
            "(* (- 123) (group 45.67))".to_string()
        );
    }
}
