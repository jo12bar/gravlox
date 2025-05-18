use std::borrow::Cow;
use std::fmt;

use log::debug;

use crate::ast::{Stmt, StmtWalkable};
use crate::environment::Environment;
use crate::literal::Literal;
use crate::{
    Lox,
    ast::{self, Expr, ExprWalkable},
    runtime_error::RuntimeError,
    token::Token,
    token_type::TokenType,
};

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Nil,
    Num(f64),
    Bool(bool),
    String(Cow<'a, str>),
}

impl Value<'_> {
    fn from_literal<'a, 'r: 'a>(literal: &'r Literal<'a>) -> Value<'a> {
        match literal {
            Literal::Num(n) => Value::Num(*n),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::String(Cow::Borrowed(s)) => Value::String(Cow::Borrowed(s)),
            Literal::String(Cow::Owned(s)) => Value::String(Cow::Borrowed(s)),
        }
    }

    fn into_owned(self) -> Value<'static> {
        match self {
            Value::Nil => Value::Nil,
            Value::Num(n) => Value::Num(n),
            Value::Bool(b) => Value::Bool(b),
            Value::String(s) => Value::String(s.into_owned().into()),
        }
    }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(num) => {
                let mut text = num.to_string();
                if text.ends_with(".0") {
                    text.pop();
                    text.pop();
                }
                text.fmt(f)
            }
            Value::Bool(b) => b.fmt(f),
            Value::String(s) => s.fmt(f),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Default)]
pub struct Interpreter {
    environment: Box<Environment>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Self::default()
    }

    /// Interpret an expression, and print the result.
    pub fn interpret(&mut self, statements: &[Stmt<'_>], lox: &mut Lox) {
        debug!("evaluating {} statements", statements.len());
        for statement in statements {
            if let Err(e) = self.execute(statement) {
                debug!(
                    "evaluating statement resulted in runtime error\nstatement: {0:?}\nruntime error: {1} ({1:?})",
                    statement, &e
                );
                lox.runtime_error(e);
                break;
            }
        }
        debug!("done evaluating {} statements", statements.len());
    }

    fn execute<'a, 'r: 'a>(&mut self, stmt: &'r Stmt<'a>) -> Result<(), RuntimeError> {
        stmt.walk_stmt(self)
    }

    fn execute_block<'a, 'r: 'a>(
        &mut self,
        statements: &'r [Stmt<'a>],
        environment: Box<Environment>,
    ) -> Result<(), RuntimeError> {
        debug!("evaluating {} statements in block", statements.len());

        self.environment = environment;

        let mut res = Ok(());
        for statement in statements {
            if let Err(e) = self.execute(statement) {
                res = Err(e);
                break;
            }
        }

        if let Some(previous) = self.environment.take_enclosing_environment() {
            self.environment = previous;
        }

        debug!("done evaluating {} statements in block", statements.len());

        res
    }

    fn evaluate<'a, 'r: 'a>(&mut self, expr: &'r Expr<'a>) -> Result<Value<'a>, RuntimeError> {
        if log::log_enabled!(log::Level::Trace) {
            log::trace!(
                "evaluating expression: {}",
                crate::ast_printer::AstPrinter(expr).walk_ast()
            );
        }
        expr.walk_expr(self)
    }
}

impl ast::ExprVisitor for Interpreter {
    type Ret<'a> = Result<Value<'a>, RuntimeError>;

    fn visit_literal_expr<'a, 'r: 'a>(
        &mut self,
        literal_expr: &'r Expr<'a>,
    ) -> Result<Value<'a>, RuntimeError> {
        let Expr::Literal(value) = literal_expr else {
            unreachable!("should always be a literal expr");
        };

        match value {
            Some(lit) => Ok(Value::from_literal(lit)),
            None => Ok(Value::Nil),
        }
    }

    fn visit_grouping_expr<'a, 'r: 'a>(
        &mut self,
        grouping_expr: &'r Expr<'a>,
    ) -> Result<Value<'a>, RuntimeError> {
        let Expr::Grouping(expression) = grouping_expr else {
            unreachable!("should always be a grouping expr");
        };
        self.evaluate(expression)
    }

    fn visit_unary_expr<'a, 'r: 'a>(
        &mut self,
        unary_expr: &'r Expr<'a>,
    ) -> Result<Value<'a>, RuntimeError> {
        let Expr::Unary { operator, right } = unary_expr else {
            unreachable!("should always be an unary expr");
        };

        let right = self.evaluate(right)?;

        match operator.typ() {
            TokenType::Minus => {
                let right = operand_as_num(operator, &right)?;
                Ok(Value::Num(-right))
            }

            TokenType::Bang => Ok(Value::Bool(!is_truthy(&right))),

            typ => {
                unreachable!("tried to evaluate invalid unary operator {typ:?}");
            }
        }
    }

    fn visit_binary_expr<'a, 'r: 'a>(
        &mut self,
        binary_expr: &'r Expr<'a>,
    ) -> Result<Value<'a>, RuntimeError> {
        let Expr::Binary {
            left,
            operator,
            right,
        } = binary_expr
        else {
            unreachable!("should always be a binary expr");
        };

        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        match operator.typ() {
            TokenType::Minus => {
                let (left, right) = operands_as_2_nums(operator, (&left, &right))?;
                Ok(Value::Num(left - right))
            }

            TokenType::Slash => {
                let (left, right) = operands_as_2_nums(operator, (&left, &right))?;
                Ok(Value::Num(left / right))
            }

            TokenType::Star => {
                let (left, right) = operands_as_2_nums(operator, (&left, &right))?;
                Ok(Value::Num(left * right))
            }

            TokenType::Plus => {
                if let (Value::Num(left), Value::Num(right)) = (&left, &right) {
                    Ok(Value::Num(left + right))
                } else if let (Value::String(left), Value::String(right)) = (left, right) {
                    let mut res_string = String::with_capacity(left.len() + right.len());
                    res_string.push_str(&left);
                    res_string.push_str(&right);
                    Ok(Value::String(Cow::from(res_string)))
                } else {
                    Err(RuntimeError::new(
                        operator.clone(),
                        "Operands must be two numbers or two strings.".to_string(),
                    ))
                }
            }

            TokenType::Greater => {
                let (left, right) = operands_as_2_nums(operator, (&left, &right))?;
                Ok(Value::Bool(left > right))
            }

            TokenType::GreaterEqual => {
                let (left, right) = operands_as_2_nums(operator, (&left, &right))?;
                Ok(Value::Bool(left >= right))
            }

            TokenType::Less => {
                let (left, right) = operands_as_2_nums(operator, (&left, &right))?;
                Ok(Value::Bool(left < right))
            }

            TokenType::LessEqual => {
                let (left, right) = operands_as_2_nums(operator, (&left, &right))?;
                Ok(Value::Bool(left <= right))
            }

            TokenType::BangEqual => Ok(Value::Bool(!is_equal(&left, &right))),
            TokenType::EqualEqual => Ok(Value::Bool(is_equal(&left, &right))),

            TokenType::Comma => Ok(right),

            typ => {
                unreachable!("tried to evaluate invalid binary operator {typ:?}");
            }
        }
    }

    fn visit_var_expr<'a, 'r: 'a>(&mut self, var_expr: &'r Expr<'a>) -> Self::Ret<'a> {
        let Expr::Var { name } = var_expr else {
            unreachable!("should always be a var expression");
        };

        self.environment.get(name).cloned()
    }

    fn visit_assign_expr<'a, 'r: 'a>(&mut self, assign_expr: &'r Expr<'a>) -> Self::Ret<'a> {
        let Expr::Assign { name, value } = assign_expr else {
            unreachable!("shoudl always be an assign expression");
        };

        let value = self.evaluate(value)?;
        self.environment.assign(name, value.clone().into_owned())?;
        Ok(value)
    }
}

impl ast::StmtVisitor for Interpreter {
    type Ret<'a> = Result<(), RuntimeError>;

    fn visit_expression_stmt<'a, 'r: 'a>(
        &mut self,
        expression_stmt: &'r Stmt<'a>,
    ) -> Self::Ret<'a> {
        let Stmt::Expression(expression) = expression_stmt else {
            unreachable!("should always be an expression statement");
        };

        self.evaluate(expression)?;
        Ok(())
    }

    fn visit_print_stmt<'a, 'r: 'a>(&mut self, print_stmt: &'r Stmt<'a>) -> Self::Ret<'a> {
        let Stmt::Print(expression) = print_stmt else {
            unreachable!("should always be a print statement");
        };

        let value = self.evaluate(expression)?;
        println!("{value}");

        Ok(())
    }

    fn visit_var_stmt<'a, 'r: 'a>(&mut self, var_stmt: &'r Stmt<'a>) -> Self::Ret<'a> {
        let Stmt::Var { name, initializer } = var_stmt else {
            unreachable!("should always be a var statement");
        };

        // Lox sets a variable to `nil` if it isn't explicitly initialized.
        let value = if let Some(expr) = initializer {
            self.evaluate(expr)?
        } else {
            Value::Nil
        };

        self.environment.define(name.lexeme(), value.into_owned());

        Ok(())
    }

    fn visit_block_stmt<'a, 'r: 'a>(&mut self, block_stmt: &'r Stmt<'a>) -> Self::Ret<'a> {
        let Stmt::Block { statements } = block_stmt else {
            unreachable!("should always be a block statement");
        };

        let enclosing_environment = std::mem::take(&mut self.environment);

        self.execute_block(
            statements,
            Box::new(Environment::new_enclosed_by(enclosing_environment)),
        )?;

        Ok(())
    }
}

fn operand_as_num(operator: &Token, operand: &Value<'_>) -> Result<f64, RuntimeError> {
    if let Value::Num(n) = operand {
        Ok(*n)
    } else {
        Err(RuntimeError::new(
            operator.clone(),
            "Operand must be a number".to_string(),
        ))
    }
}

fn operands_as_2_nums(
    operator: &Token,
    (left, right): (&Value<'_>, &Value<'_>),
) -> Result<(f64, f64), RuntimeError> {
    if let (Value::Num(left), Value::Num(right)) = (left, right) {
        Ok((*left, *right))
    } else {
        Err(RuntimeError::new(
            operator.clone(),
            "Operands must be numbers.".to_string(),
        ))
    }
}

/// `false` and `nil` are falsey, and everything else is truthy.
///
/// Note that `nil` is represented by the interpreter by `Box<()>`.
fn is_truthy(val: &Value<'_>) -> bool {
    if matches!(val, Value::Nil) {
        return false;
    }

    if let Value::Bool(b) = val {
        return *b;
    }

    true
}

fn is_equal(a: &Value<'_>, b: &Value<'_>) -> bool {
    if matches!(a, Value::Nil) && matches!(b, Value::Nil) {
        return true;
    }

    if matches!(a, Value::Nil) {
        return false;
    }

    if let (Value::Bool(a), Value::Bool(b)) = (a, b) {
        return a == b;
    }

    if let (Value::Num(a), Value::Num(b)) = (a, b) {
        return a == b;
    }

    if let (Value::String(a), Value::String(b)) = (a, b) {
        return a == b;
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_truthy_returns_false_for_nil() {
        assert!(!is_truthy(&Value::Nil));
    }

    #[test]
    fn is_truthy_returns_false_for_false() {
        assert!(!is_truthy(&Value::Bool(false)));
    }

    #[test]
    fn is_truthy_returns_true_for_true() {
        assert!(is_truthy(&Value::Bool(true)));
    }

    #[test]
    fn is_truthy_returns_true_for_truethy_values() {
        assert!(is_truthy(&Value::String(Cow::Borrowed("this is truthy"))));
        assert!(is_truthy(&Value::String(Cow::Owned(
            "so is this".to_string()
        ))));
        assert!(is_truthy(&Value::Num(-0.43)));
        assert!(is_truthy(&Value::Num(0.0)));
    }

    #[test]
    fn is_equal_strings() {
        assert!(is_equal(
            &Value::String(Cow::Borrowed("hey")),
            &Value::String(Cow::Borrowed("hey"))
        ));
        assert!(!is_equal(
            &Value::String(Cow::Borrowed("hey")),
            &Value::String(Cow::Borrowed("ooo"))
        ));

        assert!(is_equal(
            &Value::String(Cow::Owned("hey".to_string())),
            &Value::String(Cow::Borrowed("hey"))
        ));
        assert!(!is_equal(
            &Value::String(Cow::Owned("hey".to_string())),
            &Value::String(Cow::Borrowed("ooo"))
        ));

        assert!(is_equal(
            &Value::String(Cow::Borrowed("hey")),
            &Value::String(Cow::Owned("hey".to_string()))
        ));
        assert!(!is_equal(
            &Value::String(Cow::Borrowed("hey")),
            &Value::String(Cow::Owned("ooo".to_string()))
        ));

        assert!(is_equal(
            &Value::String(Cow::Owned("hey".to_string())),
            &Value::String(Cow::Owned("hey".to_string()))
        ));
        assert!(!is_equal(
            &Value::String(Cow::Owned("hey".to_string())),
            &Value::String(Cow::Owned("ooo".to_string()))
        ));

        assert!(!is_equal(
            &Value::String(Cow::Borrowed("nil test")),
            &Value::Nil
        ));
        assert!(!is_equal(
            &Value::String(Cow::Owned("nil test".to_string())),
            &Value::Nil
        ));
        assert!(!is_equal(
            &Value::Nil,
            &Value::String(Cow::Borrowed("nil test"))
        ));
        assert!(!is_equal(
            &Value::Nil,
            &Value::String(Cow::Owned("nil test".to_string()))
        ));
    }

    #[test]
    fn is_equal_bools() {
        assert!(is_equal(&Value::Bool(false), &Value::Bool(false)));
        assert!(!is_equal(&Value::Bool(false), &Value::Bool(true)));
        assert!(!is_equal(&Value::Bool(true), &Value::Bool(false)));
        assert!(is_equal(&Value::Bool(true), &Value::Bool(true)));

        assert!(!is_equal(&Value::Nil, &Value::Bool(false)));
        assert!(!is_equal(&Value::Nil, &Value::Bool(true)));
        assert!(!is_equal(&Value::Bool(false), &Value::Nil));
        assert!(!is_equal(&Value::Bool(true), &Value::Nil));
    }

    #[test]
    fn is_equal_nums() {
        assert!(is_equal(&Value::Num(420.0_f64), &Value::Num(420.0_f64)));
        assert!(!is_equal(&Value::Num(420.0_f64), &Value::Num(-6.9_f64)));

        assert!(!is_equal(&Value::Nil, &Value::Num(420.0_f64)));
        assert!(!is_equal(&Value::Nil, &Value::Num(420.0_f64)));
        assert!(!is_equal(&Value::Num(420.0_f64), &Value::Nil));
        assert!(!is_equal(&Value::Num(420.0_f64), &Value::Nil));
    }

    #[test]
    fn is_equal_nil() {
        assert!(is_equal(&Value::Nil, &Value::Nil));
    }
}
