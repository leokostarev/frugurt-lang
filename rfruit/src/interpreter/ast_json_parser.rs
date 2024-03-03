use super::{FruExpression, FruField, FruStatement, FruValue, Identifier, TypeType};
use serde_json::Value;
use std::collections::HashMap;
use std::rc::Rc;

pub fn parse(data: Value) -> Box<FruStatement> {
    Box::new(convert(&data).as_stmt())
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

fn convert(ast: &Value) -> Anything {
    use Anything::Expression as Expr;
    use Anything::Statement as Stmt;

    let t = ast["node"].as_str().unwrap();

    match t {
        // statements
        "composite" => {
            let body = ast["body"].as_array().unwrap();

            Stmt(FruStatement::Composite(
                body.iter().map(|x| convert(x).as_stmt()).collect(),
            ))
        }

        "expression" => {
            let value = convert(&ast["value"]).as_expr();
            Stmt(FruStatement::Expression {
                value: Box::new(value),
            })
        }

        "let" => {
            let ident = ast["ident"].as_str().unwrap();
            let value = convert(&ast["value"]).as_expr();

            Stmt(FruStatement::Let {
                ident: Identifier::new(ident),
                value: Box::new(value),
            })
        }

        "set" => {
            let ident = ast["ident"].as_str().unwrap();
            let value = convert(&ast["value"]).as_expr();

            Stmt(FruStatement::Set {
                ident: Identifier::new(ident),
                value: Box::new(value),
            })
        }

        "if" => {
            let cond = convert(&ast["cond"]).as_expr();
            let then = convert(&ast["then"]).as_stmt();
            let else_ = convert(&ast["else"]).as_stmt();

            Stmt(FruStatement::If {
                cond: Box::new(cond),
                then_body: Box::new(then),
                else_body: Box::new(else_),
            })
        }

        "while" => {
            let cond = convert(&ast["cond"]).as_expr();
            let body = convert(&ast["body"]).as_stmt();

            Stmt(FruStatement::While {
                cond: Box::new(cond),
                body: Box::new(body),
            })
        }

        "return" => {
            let value = convert(&ast["value"]).as_expr();

            Stmt(FruStatement::Return {
                value: Box::new(value),
            })
        }

        "block_return" => {
            let value = convert(&ast["value"]).as_expr();

            Stmt(FruStatement::BlockReturn {
                value: Box::new(value),
            })
        }

        "break" => Stmt(FruStatement::Break),

        "continue" => Stmt(FruStatement::Continue),

        "operator" => {
            let ident = ast["ident"].as_str().unwrap();
            let left_arg = ast["left_arg"].as_str().unwrap();
            let left_type = ast["left_type"].as_str().unwrap();
            let right_arg = ast["right_arg"].as_str().unwrap();
            let right_type = ast["right_type"].as_str().unwrap();
            let body = convert(&ast["body"]).as_stmt();

            Stmt(FruStatement::OperatorDefinition {
                ident: Identifier::new(ident),
                left_arg: Identifier::new(left_arg),
                left_type: Identifier::new(left_type),
                right_arg: Identifier::new(right_arg),
                right_type: Identifier::new(right_type),
                body: Rc::new(body),
            })
        }

        "type" => {
            let type_ = ast["type"].as_str().unwrap();
            let ident = ast["ident"].as_str().unwrap();
            let fields = ast["fields"]
                .as_array()
                .unwrap()
                .iter()
                .map(convert_fru_field)
                .collect();
            let raw_watches: Vec<(Vec<Identifier>, Rc<FruStatement>)> = ast["watches"]
                .as_array()
                .unwrap()
                .iter()
                .map(convert_raw_watch)
                .collect();

            let mut watches_by_field: HashMap<Identifier, Vec<Rc<FruStatement>>> = HashMap::new();
            let mut watches = Vec::new();

            for (watch_fields, watch_body) in raw_watches {
                watches.push(watch_body.clone());

                for field in watch_fields {
                    if let Some(body) = watches_by_field.get_mut(&field) {
                        body.push(watch_body.clone());
                    } else {
                        watches_by_field.insert(field, vec![watch_body.clone()]);
                    }
                }
            }

            Stmt(FruStatement::TypeDeclaration {
                type_: match type_ {
                    "struct" => TypeType::Struct,
                    other => panic!("only structs are supported now, not {}", other),
                },
                ident: Identifier::new(ident),
                fields,
                watches_by_field,
                watches,
            })
        }

        // expressions
        "literal" => match &ast["value"] {
            Value::Number(n) => {
                if n.is_i64() {
                    Expr(FruExpression::Literal(FruValue::Number(
                        n.as_f64().unwrap(),
                    )))
                } else if n.is_f64() {
                    Expr(FruExpression::Literal(FruValue::Number(
                        n.as_f64().unwrap(),
                    )))
                } else {
                    panic!("json is invalid");
                }
            }

            Value::Bool(v) => Expr(FruExpression::Literal(FruValue::Bool(*v))),

            Value::String(s) => Expr(FruExpression::Literal(FruValue::String(s.clone()))),

            _ => panic!("json is invalid"),
        },

        "variable" => {
            let ident = ast["ident"].as_str().unwrap();

            Expr(FruExpression::Variable(Identifier::new(ident)))
        }

        "call" => {
            let what = convert(&ast["what"]).as_expr();
            let args = ast["args"]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| convert(x).as_expr())
                .collect();

            Expr(FruExpression::Call {
                what: Box::new(what),
                args,
            })
        }

        "curry" => {
            let what = convert(&ast["what"]).as_expr();
            let args = ast["args"]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| convert(x).as_expr())
                .collect();

            Expr(FruExpression::CurryCall {
                what: Box::new(what),
                args,
            })
        }

        "binary" => {
            let operator = ast["operator"].as_str().unwrap();
            let left = convert(&ast["left"]).as_expr();
            let right = convert(&ast["right"]).as_expr();

            Expr(FruExpression::Binary {
                operator: Identifier::new(operator),
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        "function" => {
            let args = ast["args"]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| x.as_str().unwrap().to_string())
                .map(|x| Identifier::new(&x))
                .collect();
            let body = convert(&ast["body"]).as_stmt();

            Expr(FruExpression::Function {
                args,
                body: Rc::new(body),
            })
        }

        "instantiation" => {
            let what = convert(&ast["what"]).as_expr();
            let args = ast["args"]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| convert(x).as_expr())
                .collect();

            Expr(FruExpression::Instantiation {
                what: Box::new(what),
                args,
            })
        }

        "field_access" => {
            let what = convert(&ast["what"]).as_expr();
            let field = ast["field"].as_str().unwrap();

            Expr(FruExpression::FieldAccess {
                what: Box::new(what),
                field: Identifier::new(field),
            })
        }

        unknown => panic!("unknown node: {}", unknown),
    }
}

fn convert_fru_field(ast: &Value) -> FruField {
    let ident = ast["ident"].as_str().unwrap();
    let is_public = ast["is_pub"].as_bool().unwrap();
    FruField {
        ident: Identifier::new(ident),
        is_public,
    }
}

fn convert_raw_watch(ast: &Value) -> (Vec<Identifier>, Rc<FruStatement>) {
    let fields = ast["fields"]
        .as_array()
        .unwrap()
        .iter()
        .map(|x| Identifier::new(x.as_str().unwrap()))
        .collect();
    let body = convert(&ast["body"]).as_stmt();
    (fields, Rc::new(body))
}
