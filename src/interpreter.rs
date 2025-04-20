use std::borrow::Cow;

use crate::literal::Literal;
use crate::{
    Lox,
    expr::{self, Expr, Walkable},
    runtime_error::RuntimeError,
    token::Token,
    token_type::TokenType,
};

#[derive(Debug, Clone)]
enum Value<'a> {
    Nil,
    Num(f64),
    Bool(bool),
    String(Cow<'a, str>),
}

impl Value<'_> {
    fn from_literal(literal: &dyn Literal) -> Value<'_> {
        // TODO: This is awful, need to turn literal into an enum and unify this somehow
        let literal = literal.as_any();

        if literal.downcast_ref::<()>().is_some() {
            Value::Nil
        } else if let Some(num) = literal.downcast_ref::<f64>() {
            Value::Num(*num)
        } else if let Some(b) = literal.downcast_ref::<bool>() {
            Value::Bool(*b)
        } else if let Some(s) = literal.downcast_ref::<&str>() {
            Value::String(Cow::Borrowed(s))
        } else if let Some(s) = literal.downcast_ref::<String>() {
            Value::String(Cow::Owned(s.clone()))
        } else {
            unimplemented!(
                "Interpreter bug: tried to convert unhandled literal type {:?} into an interpreter value",
                (*literal).type_id()
            );
        }
    }

    fn from_owned_literal(literal: Box<dyn Literal>) -> Value<'static> {
        // TODO: This is awful, need to turn literal into an enum and unify this somehow
        let literal = literal.clone();
        let literal = literal.as_any();

        if literal.downcast_ref::<()>().is_some() {
            Value::Nil
        } else if let Some(num) = literal.downcast_ref::<f64>() {
            Value::Num(*num)
        } else if let Some(b) = literal.downcast_ref::<bool>() {
            Value::Bool(*b)
        } else if let Some(s) = literal.downcast_ref::<&str>() {
            Value::String(Cow::Borrowed(s))
        } else if let Some(s) = literal.downcast_ref::<String>() {
            Value::String(Cow::Owned(s.clone()))
        } else {
            unimplemented!(
                "Interpreter bug: tried to convert unhandled literal type {:?} into an interpreter value",
                (*literal).type_id()
            );
        }
    }
}

#[derive(Debug)]
pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter
    }

    /// Interpret an expression, and print the result.
    pub fn interpret(&mut self, expr: &Expr, lox: &mut Lox) {
        match self.evaluate(expr) {
            Ok(res_val) => match res_val {
                Value::Num(num) => {
                    let mut text = num.to_string();
                    if text.ends_with(".0") {
                        text.pop();
                        text.pop();
                    }
                    println!("-- f64 --");
                    println!("{num}");
                }
                Value::Bool(b) => {
                    println!("-- bool --");
                    println!("{b}");
                }
                Value::String(s) => {
                    if matches!(s, Cow::Borrowed(_)) {
                        println!("-- Cow<'_, str>::Borrowed --");
                    } else {
                        println!("-- Cow<'_, str>::Owned --");
                    }
                    println!("{s}");
                }
                Value::Nil => {
                    println!("nil");
                }
            },

            Err(e) => {
                lox.runtime_error(e);
            }
        }
    }

    fn evaluate<'a>(&mut self, expr: &Expr) -> Result<Value<'a>, RuntimeError> {
        expr.walk(self)
    }
}

impl<'a> expr::Visitor<Result<Value<'a>, RuntimeError>> for Interpreter {
    fn visit_literal_expr(&mut self, literal_expr: &Expr) -> Result<Value<'a>, RuntimeError> {
        let Expr::Literal(value) = literal_expr else {
            unreachable!("should always be a literal expr");
        };

        match value {
            Some(lit) => Ok(Value::from_owned_literal(lit.clone())),
            None => Ok(Value::Nil),
        }
    }

    fn visit_grouping_expr(&mut self, grouping_expr: &Expr) -> Result<Value<'a>, RuntimeError> {
        let Expr::Grouping(expression) = grouping_expr else {
            unreachable!("should always be a grouping expr");
        };
        self.evaluate(expression)
    }

    fn visit_unary_expr(&mut self, unary_expr: &Expr) -> Result<Value<'a>, RuntimeError> {
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

    fn visit_binary_expr(&mut self, binary_expr: &Expr) -> Result<Value<'a>, RuntimeError> {
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

            typ => {
                unreachable!("tried to evaluate invalid binary operator {typ:?}");
            }
        }
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
