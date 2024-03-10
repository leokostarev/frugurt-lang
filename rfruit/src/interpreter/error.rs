use crate::FruValue;

#[derive(Debug)]
pub struct FruError {
    message: String,
}

impl FruError {
    pub fn new(message: String) -> FruError {
        FruError { message }
    }

    pub fn new_slice(message: &str) -> FruError {
        FruError {
            message: message.to_string(),
        }
    }

    pub fn new_res(message: String) -> Result<FruValue, FruError> {
        Err(FruError { message })
    }

    pub fn new_res_slice(message: &str) -> Result<FruValue, FruError> {
        Err(FruError {
            message: message.to_string(),
        })
    }

    pub fn new_res_err(message: String) -> Result<(), FruError> {
        Err(FruError { message })
    }

    pub fn new_res_err_slice(message: &str) -> Result<(), FruError> {
        Err(FruError {
            message: message.to_string(),
        })
    }
}
