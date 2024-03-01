use super::{
    AnyOperator, FruError, FruExpression, FruStructType, FruValue, Identifier, OperatorIdentifier,
    Scope,
};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum FruStatement {
    Composite(Vec<FruStatement>),
    Expression {
        value: Box<FruExpression>,
    },
    Let {
        ident: Identifier,
        value: Box<FruExpression>,
    },
    Set {
        ident: Identifier,
        value: Box<FruExpression>,
    },
    If {
        cond: Box<FruExpression>,
        then_body: Box<FruStatement>,
        else_body: Box<FruStatement>,
    },
    While {
        cond: Box<FruExpression>,
        body: Box<FruStatement>,
    },
    Return {
        value: Box<FruExpression>,
    },
    BlockReturn {
        value: Box<FruExpression>,
    },
    Break,
    Continue,
    OperatorDefinition {
        ident: Identifier,
        left_arg: Identifier,
        left_type: Identifier,
        right_arg: Identifier,
        right_type: Identifier,
        body: Rc<FruStatement>,
    },
    TypeDeclaration {
        type_: TypeType,
        ident: Identifier,
        fields: Vec<Identifier>,
    },
}

#[derive(Debug, Clone)]
pub enum TypeType {
    Struct, // class, data is coming
}

#[derive(Debug)]
pub enum StatementSignal {
    Nah,
    Continue,
    Break,
    Return(FruValue),
    BlockReturn(FruValue),
}

impl FruStatement {
    pub fn execute(&self, scope: Rc<Scope>) -> Result<StatementSignal, FruError> {
        match self {
            FruStatement::Composite(statements) => {
                for statement in statements {
                    match statement.execute(scope.clone()) {
                        Ok(StatementSignal::Nah) => {}

                        Ok(code) => return Ok(code),

                        Err(err) => return Err(err),
                    }
                }

                Ok(StatementSignal::Nah)
            }

            FruStatement::Expression { value: expr } => {
                expr.evaluate(scope.clone())?;
                Ok(StatementSignal::Nah)
            }

            FruStatement::Let { ident, value } => {
                let v = value.evaluate(scope.clone())?;
                scope.let_variable(*ident, v)?;
                Ok(StatementSignal::Nah)
            }

            FruStatement::Set { ident, value } => {
                let v = value.evaluate(scope.clone())?;
                scope.set_variable(*ident, v)?;
                Ok(StatementSignal::Nah)
            }

            FruStatement::If {
                cond: condition,
                then_body: then,
                else_body: else_,
            } => {
                let result = condition.evaluate(scope.clone())?;
                if let FruValue::Bool(b) = result {
                    if b {
                        then.execute(scope.clone())
                    } else {
                        else_.execute(scope.clone())
                    }
                } else {
                    Err(FruError::new(format!(
                        "{} is not a boolean",
                        result.get_type_identifier()
                    )))
                }
            }

            FruStatement::While {
                cond: condition,
                body,
            } => loop {
                let result = condition.evaluate(scope.clone())?;
                match result {
                    FruValue::Bool(b) => {
                        if b {
                            let res = body.execute(scope.clone())?;

                            match res {
                                StatementSignal::Nah => {}
                                StatementSignal::Continue => continue,
                                StatementSignal::Break => return Ok(StatementSignal::Nah),
                                StatementSignal::Return(v) => {
                                    return Ok(StatementSignal::Return(v));
                                }
                                StatementSignal::BlockReturn(_) => {
                                    return Err(FruError::news("block return in while loop"));
                                }
                            }
                        } else {
                            return Ok(StatementSignal::Nah);
                        }
                    }
                    _ => {
                        return Err(FruError::news("condition is not a boolean"));
                    }
                }
            },

            FruStatement::Return { value } => {
                let v = value.evaluate(scope)?;
                return Ok(StatementSignal::Return(v));
            }

            FruStatement::BlockReturn { value } => {
                let v = value.evaluate(scope)?;
                return Ok(StatementSignal::BlockReturn(v));
            }

            FruStatement::Break => Ok(StatementSignal::Break),
            FruStatement::Continue => Ok(StatementSignal::Continue),

            FruStatement::OperatorDefinition {
                ident,
                left_arg,
                left_type,
                right_arg,
                right_type,
                body,
            } => {
                scope.set_operator(
                    OperatorIdentifier::new(*ident, *left_type, *right_type),
                    AnyOperator::Operator {
                        left_ident: *left_arg,
                        right_ident: *right_arg,
                        body: body.clone(),
                        scope: scope.clone(),
                    },
                );

                Ok(StatementSignal::Nah)
            }
            FruStatement::TypeDeclaration {
                type_,
                ident,
                fields,
            } => {
                scope.let_variable(
                    *ident,
                    match type_ {
                        TypeType::Struct => FruValue::StructType(Rc::new(FruStructType {
                            name: *ident,
                            fields: fields.clone(),
                        })),
                    },
                )?;

                Ok(StatementSignal::Nah)
            }
        }
    }
}
