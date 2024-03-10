use crate::interpreter::{FruError, FruValue, StatementSignal};
use crate::{FruStatement, FruType, Scope};
use std::rc::{Rc, Weak};

#[derive(Clone)]
pub struct FruWatch {
    pub body: Rc<FruStatement>,
    pub type_: Weak<FruType>,
}

impl FruWatch {
    pub fn run(&self, scope: Rc<Scope>) -> Result<(), FruError> {
        let signal = self.body.execute(scope)?;

        match signal {
            StatementSignal::Nah | StatementSignal::Return(FruValue::None) => Ok(()),
            other => FruError::new_unit(format!("unexpected signal {:?}", other)),
        }
    }
}
