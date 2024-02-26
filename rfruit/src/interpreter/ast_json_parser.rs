use crate::interpreter::*;
use serde_json::Value;
use std::rc::Rc;

pub fn parse(data: Value) -> Box<FruStatement> {
    Box::new(convert(data).as_stmt())
}

enum Anything {
    Statement(FruStatement),
    Expression(FruExpression),
}

impl Anything {
    fn as_stmt(&self) -> FruStatement {
        match self {
            Anything::Statement(v) => v.clone(),
            Anything::Expression(x) => panic!("{:?} is not a statement", x),
        }
    }

    fn as_expr(&self) -> FruExpression {
        match self {
            Anything::Expression(v) => v.clone(),
            Anything::Statement(x) => panic!("{:?} is not an expression", x),
        }
    }
}

fn convert(ast: Value) -> Anything {
    use Anything::Expression as Expr;
    use Anything::Statement as Stmt;

    let t = ast["node"].as_str().unwrap();

    match t {
        // statements
        "composite" => {
            let body = ast["body"].as_array().unwrap();

            Stmt(FruStatement::Composite(
                body.iter().map(|x| convert(x.clone()).as_stmt()).collect(),
            ))
        }

        "expression" => {
            let value = convert(ast["value"].clone()).as_expr();
            Stmt(FruStatement::Expression { value: Box::new(value) })
        }

        "let" => {
            let ident = ast["ident"].as_str().unwrap();
            let value = convert(ast["value"].clone()).as_expr();

            Stmt(FruStatement::Let {
                ident: Identifier::new(ident),
                value: Box::new(value),
            })
        }

        "set" => {
            let ident = ast["ident"].as_str().unwrap();
            let value = convert(ast["value"].clone()).as_expr();

            Stmt(FruStatement::Set {
                ident: Identifier::new(ident),
                value: Box::new(value),
            })
        }

        "if" => {
            let cond = convert(ast["cond"].clone()).as_expr();
            let then = convert(ast["then"].clone()).as_stmt();
            let else_ = convert(ast["else"].clone()).as_stmt();

            Stmt(FruStatement::If {
                cond: Box::new(cond),
                then: Box::new(then),
                else_: Box::new(else_),
            })
        }

        "while" => {
            let cond = convert(ast["cond"].clone()).as_expr();
            let body = convert(ast["body"].clone()).as_stmt();

            Stmt(FruStatement::While {
                cond: Box::new(cond),
                body: Box::new(body),
            })
        }

        "return" => {
            let value = convert(ast["value"].clone()).as_expr();

            Stmt(FruStatement::Return {
                value: Box::new(value),
            })
        }

        // expressions
        "literal" => match ast["value"].clone() {
            Value::Number(n) => {
                if n.is_i64() {
                    Expr(FruExpression::Literal(FruValue::Int(n.as_i64().unwrap())))
                } else if n.is_f64() {
                    Expr(FruExpression::Literal(FruValue::Float(n.as_f64().unwrap())))
                } else {
                    panic!("json is invalid");
                }
            }

            Value::Bool(v) => Expr(FruExpression::Literal(FruValue::Bool(v))),

            _ => panic!("json is invalid"),
        },

        "variable" => {
            let ident = ast["ident"].as_str().unwrap();

            Expr(FruExpression::Variable(Identifier::new(ident)))
        }

        "call" => {
            let what = convert(ast["what"].clone()).as_expr();
            let args = ast["args"]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| convert(x.clone()).as_expr())
                .collect();

            Expr(FruExpression::Call {
                what: Box::new(what),
                args,
            })
        }

        "binary" => {
            let operator = ast["operator"].as_str().unwrap();
            let left = convert(ast["left"].clone()).as_expr();
            let right = convert(ast["right"].clone()).as_expr();

            Expr(FruExpression::Binary {
                operator: Identifier::new(operator),
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        "fn_def" => {
            let args = ast["args"]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| x.as_str().unwrap().to_string())
                .map(|x| Identifier::new(&x))
                .collect();
            let body = convert(ast["body"].clone()).as_stmt();

            Expr(FruExpression::FnDef {
                args,
                body: Rc::new(body),
            })
        }

        unknown => panic!("unknown node: {}", unknown),
    }
}
