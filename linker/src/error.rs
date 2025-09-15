use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub struct LinkerError(pub String);

impl Display for LinkerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Linker Error: {}", self.0)
    }
}

impl Error for LinkerError {}
pub type LinkerResult<T> = Result<T, LinkerError>;
