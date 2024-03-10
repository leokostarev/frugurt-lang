use crate::{FruValue, StatementSignal};

#[derive(Debug)]
pub struct FruError {
    message: String,
}

impl FruError {
    pub fn new(message: String) -> FruError {
        FruError { message }
    }

    pub fn new_val(message: String) -> Result<FruValue, FruError> {
        Err(FruError { message })
    }

    pub fn new_val_slice(message: &str) -> Result<FruValue, FruError> {
        Err(FruError {
            message: message.to_string(),
        })
    }

    pub fn new_unit(message: String) -> Result<(), FruError> {
        Err(FruError { message })
    }

    pub fn new_signal(message: String) -> Result<StatementSignal, FruError> {
        Err(FruError { message })
    }
}
