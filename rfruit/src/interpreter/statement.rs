use super::{FruError, FruExpression, FruValue, Identifier, Scope};
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
        then: Box<FruStatement>,
        else_: Box<FruStatement>,
    },
    While {
        cond: Box<FruExpression>,
        body: Box<FruStatement>,
    },
    Return {
        value: Box<FruExpression>,
    },
    Break,
    Continue,
}

#[derive(Debug)]
pub enum StatementSignal {
    Nah,
    Continue,
    Break,
    Return(FruValue),
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
                then,
                else_,
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
                if let FruValue::Bool(b) = result {
                    if b {
                        let res = body.execute(scope.clone())?;

                        match res {
                            StatementSignal::Nah => {}
                            StatementSignal::Continue => continue,
                            StatementSignal::Break => return Ok(StatementSignal::Nah),
                            StatementSignal::Return(v) => {
                                return Ok(StatementSignal::Return(v));
                            }
                        }
                    } else {
                        return Ok(StatementSignal::Nah);
                    };
                } else {
                    return Err(FruError::news("condition is not a boolean"));
                }
            },

            FruStatement::Return { value } => {
                let v = value.evaluate(scope)?;

                return Ok(StatementSignal::Return(v));
            }

            FruStatement::Break => Ok(StatementSignal::Break),
            FruStatement::Continue => Ok(StatementSignal::Continue),
        }
    }
}
