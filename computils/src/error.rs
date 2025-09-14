#[cfg(feature = "compiler")]
use crate::lexer::{Slice, Token};
#[cfg(feature = "compiler")]
use crate::semantic::Type;

use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum CompilerError<'ip> {
    #[cfg(feature = "compiler")]
    UnknownChar { ch: char, slice: Slice<'ip> },
    #[cfg(feature = "compiler")]
    UnexpectedToken {
        got: Token<'ip>,
        expected: &'static str,
    },
    #[cfg(feature = "compiler")]
    UnexpectedType {
        got: Type,
        expected: String,
        slice: Slice<'ip>,
    },
    #[cfg(feature = "compiler")]
    UnexpectedEof,
    #[cfg(feature = "compiler")]
    OpTypeError {
        op: Token<'ip>,
        lhs: Option<Token<'ip>>,
        rhs: Token<'ip>,
    },

    #[cfg(feature = "compiler")]
    TypeError(String, Slice<'ip>),
    #[cfg(feature = "compiler")]
    UndefinedIdentifier(&'ip Token<'ip>),
    #[cfg(feature = "compiler")]
    Semantic { err: String, slice: Slice<'ip> },

    #[cfg(feature = "assembler")]
    Assembler { msg: String, line: Option<usize> },
    #[cfg(feature = "linker")]
    Linker(String),
}

impl<'ip> Display for CompilerError<'ip> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownChar { ch, slice } => {
                write!(
                    f,
                    "LexerError at {}: unknown character '{}'",
                    slice.get_row_col(),
                    ch
                )
            }
            Self::UnexpectedToken { got, expected } => {
                write!(
                    f,
                    "ParserError at {}: expected '{}' but found token '{:?}'",
                    got.slice.get_row_col(),
                    expected,
                    got.kind
                )
            }
            Self::UnexpectedType {
                got,
                expected,
                slice,
            } => {
                write!(
                    f,
                    "TypeError at {}: expected '{}' but found '{:?}'",
                    slice.get_row_col(),
                    expected,
                    got
                )
            }
            Self::UnexpectedEof => {
                write!(f, "ParserError: unexpected end of file")
            }
            Self::TypeError(err, slice) => {
                write!(f, "TypeError at {}: {}", slice.get_row_col(), err)
            }
            Self::OpTypeError { op, lhs, rhs } => {
                if let Some(lhs) = lhs {
                    write!(
                        f,
                        "TypeError at {}: cannot apply operator '{}' between '{}' and '{}'",
                        op.slice.get_row_col(),
                        op.slice.get_str(),
                        lhs.slice.get_str(),
                        rhs.slice.get_str(),
                    )
                } else {
                    write!(
                        f,
                        "TypeError at {}: cannot apply operator '{}' to '{}'",
                        op.slice.get_row_col(),
                        op.slice.get_str(),
                        rhs.slice.get_str(),
                    )
                }
            }
            Self::UndefinedIdentifier(ident) => {
                write!(
                    f,
                    "NameError at {}: undefined identifier '{}'",
                    ident.slice.get_row_col(),
                    ident.slice.get_str()
                )
            }
            Self::Semantic { err, slice } => {
                write!(f, "Semantic Error at {}: {}", slice.get_row_col(), err)
            }
            #[cfg(feature = "linker")]
            Self::Linker(err) => {
                write!(f, "Linker Error: {}", err)
            }
            #[cfg(feature = "assembler")]
            CompilerError::Assembler { msg, line } => match line {
                Some(l) => {
                    write!(f, "Assembler Error at line {}: {}", l, msg)
                }
                None => {
                    write!(f, "Assembler Error: {}", msg)
                }
            },
        }
    }
}

impl<'ip> Error for CompilerError<'ip> {}
pub type CompilerResult<'ip, T> = Result<T, CompilerError<'ip>>;
