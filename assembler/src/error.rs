use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub struct AssemblerError {
    pub msg: String,
    pub line: Option<usize>,
}

impl Display for AssemblerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.line {
            Some(l) => {
                write!(f, "Assembler Error at line {}: {}", l, self.msg)
            }
            None => {
                write!(f, "Assembler Error: {}", self.msg)
            }
        }
    }
}

impl Error for AssemblerError {}
pub type AssemblerResult<T> = Result<T, AssemblerError>;
