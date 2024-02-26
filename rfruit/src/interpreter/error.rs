use super::FruValue;

#[derive(Debug)]
pub struct FruError {
    message: String,
}

impl FruError {
    pub fn new_err<'a>(message: String) -> Result<FruValue, FruError> {
        // never Ok!
        Err(FruError { message })
    }

    pub fn new_errs<'a>(message: &str) -> Result<FruValue, FruError> {
        Err(FruError {
            message: message.to_string(),
        })
    }

    pub fn new(message: String) -> FruError {
        FruError { message }
    }
}