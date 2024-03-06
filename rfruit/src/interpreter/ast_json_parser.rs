use super::{FruExpression, FruField, FruStatement, FruValue, Identifier, TypeType};
use serde_json::Value;
use std::collections::HashMap;
use std::rc::Rc;

pub fn parse(data: Value) -> Box<FruStatement> {
    Box::new(convert_to_stmt(&data))
}

fn convert_to_expr(ast: &Value) -> FruExpression {
    let t = ast["node"].as_str().unwrap();

    match t {
        "literal" => match &ast["value"] {
            Value::Number(n) => {
                if n.is_i64() {
                    FruExpression::Literal(FruValue::Number(n.as_f64().unwrap()))
                } else if n.is_f64() {
                    FruExpression::Literal(FruValue::Number(n.as_f64().unwrap()))
                } else {
                    panic!("json is invalid");
                }
            }

            Value::Bool(v) => FruExpression::Literal(FruValue::Bool(*v)),

            Value::String(s) => FruExpression::Literal(FruValue::String(s.clone())),

            _ => panic!("json is invalid"),
        },

        "variable" => {
            let ident = ast["ident"].as_str().unwrap();

            FruExpression::Variable(Identifier::new(ident))
        }

        "call" => {
            let what = convert_to_expr(&ast["what"]);
            let args = ast["args"]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| convert_to_expr(x))
                .collect();

            (FruExpression::Call {
                what: Box::new(what),
                args,
            })
        }

        "curry" => {
            let what = convert_to_expr(&ast["what"]);
            let args = ast["args"]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| convert_to_expr(x))
                .collect();

            (FruExpression::CurryCall {
                what: Box::new(what),
                args,
            })
        }

        "binary" => {
            let operator = ast["operator"].as_str().unwrap();
            let left = convert_to_expr(&ast["left"]);
            let right = convert_to_expr(&ast["right"]);

            (FruExpression::Binary {
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
            let body = convert_to_stmt(&ast["body"]);

            (FruExpression::Function {
                args,
                body: Rc::new(body),
            })
        }

        "instantiation" => {
            let what = convert_to_expr(&ast["what"]);
            let args = ast["args"]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| convert_to_expr(x))
                .collect();

            (FruExpression::Instantiation {
                what: Box::new(what),
                args,
            })
        }

        "field_access" => {
            let what = convert_to_expr(&ast["what"]);
            let field = ast["field"].as_str().unwrap();

            (FruExpression::FieldAccess {
                what: Box::new(what),
                field: Identifier::new(field),
            })
        }

        unknown => panic!("unknown node: {}", unknown),
    }
}

fn convert_to_stmt(ast: &Value) -> FruStatement {
    let t = ast["node"].as_str().unwrap();

    match t {
        "composite" => {
            let body = ast["body"].as_array().unwrap();

            FruStatement::Composite(body.iter().map(|x| convert_to_stmt(x)).collect())
        }

        "expression" => {
            let value = convert_to_expr(&ast["value"]);
            FruStatement::Expression {
                value: Box::new(value),
            }
        }

        "let" => {
            let ident = ast["ident"].as_str().unwrap();
            let value = convert_to_expr(&ast["value"]);

            FruStatement::Let {
                ident: Identifier::new(ident),
                value: Box::new(value),
            }
        }

        "set" => {
            let path = ast["ident"]
                .as_array()
                .unwrap()
                .iter()
                .map(|x| Identifier::new(x.as_str().unwrap()))
                .collect::<Vec<_>>();
            let value = convert_to_expr(&ast["value"]);

            FruStatement::Set {
                path,
                value: Box::new(value),
            }
        }

        "if" => {
            let cond = convert_to_expr(&ast["cond"]);
            let then = convert_to_stmt(&ast["then"]);
            let else_ = convert_to_stmt(&ast["else"]);

            FruStatement::If {
                cond: Box::new(cond),
                then_body: Box::new(then),
                else_body: Box::new(else_),
            }
        }

        "while" => {
            let cond = convert_to_expr(&ast["cond"]);
            let body = convert_to_stmt(&ast["body"]);

            FruStatement::While {
                cond: Box::new(cond),
                body: Box::new(body),
            }
        }

        "return" => {
            let value = convert_to_expr(&ast["value"]);

            FruStatement::Return {
                value: Box::new(value),
            }
        }

        "block_return" => {
            let value = convert_to_expr(&ast["value"]);

            FruStatement::BlockReturn {
                value: Box::new(value),
            }
        }

        "break" => FruStatement::Break,

        "continue" => FruStatement::Continue,

        "operator" => {
            let ident = ast["ident"].as_str().unwrap();
            let left_arg = ast["left_arg"].as_str().unwrap();
            let left_type = ast["left_type"].as_str().unwrap();
            let right_arg = ast["right_arg"].as_str().unwrap();
            let right_type = ast["right_type"].as_str().unwrap();
            let body = convert_to_stmt(&ast["body"]);

            FruStatement::OperatorDefinition {
                ident: Identifier::new(ident),
                left_arg: Identifier::new(left_arg),
                left_type: Identifier::new(left_type),
                right_arg: Identifier::new(right_arg),
                right_type: Identifier::new(right_type),
                body: Rc::new(body),
            }
        }

        "type" => {
            let type_ = ast["type"].as_str().unwrap();
            let ident = ast["ident"].as_str().unwrap();
            let fields = ast["fields"]
                .as_array()
                .unwrap()
                .iter()
                .map(convert_to_fru_field)
                .collect();
            let raw_watches: Vec<(Vec<Identifier>, Rc<FruStatement>)> = ast["watches"]
                .as_array()
                .unwrap()
                .iter()
                .map(convert_to_raw_watch)
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

            FruStatement::TypeDeclaration {
                type_: match type_ {
                    "struct" => TypeType::Struct,
                    other => panic!("only structs are supported now, not {}", other),
                },
                ident: Identifier::new(ident),
                fields,
                watches_by_field,
                watches,
            }
        }
        x => panic!("{:?} is not an expression", x),
    }
}

fn convert_to_fru_field(ast: &Value) -> FruField {
    let ident = ast["ident"].as_str().unwrap();
    let is_public = ast["is_pub"].as_bool().unwrap();
    FruField {
        ident: Identifier::new(ident),
        is_public,
    }
}

fn convert_to_raw_watch(ast: &Value) -> (Vec<Identifier>, Rc<FruStatement>) {
    let fields = ast["fields"]
        .as_array()
        .unwrap()
        .iter()
        .map(|x| Identifier::new(x.as_str().unwrap()))
        .collect();
    let body = convert_to_stmt(&ast["body"]);
    (fields, Rc::new(body))
}
