use std::error::Error;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum LoxError {
    Errors(Vec<LoxError>),
    UnknownError,
    RuntimeError(String),
    CompileError {
        message: String,
        location: String,
        line: usize,
    },
    ParseError {
        message: String,
        location: String,
        line: usize,
    },
}
impl Error for LoxError {}

impl Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxError::Errors(errors) => Ok(for error in errors {
                writeln!(f, "{}", error)?;
            }),
            LoxError::UnknownError => write!(f, "Unknown error"),
            LoxError::RuntimeError(s) => write!(f, "{}", s),
            LoxError::CompileError {
                message,
                location,
                line,
            } => write!(f, "[line {}] CompileError{}: {}", line, location, message),
            LoxError::ParseError {
                message,
                location,
                line,
            } => write!(f, "[line {}] ParseError{}: {}", line, location, message),
        }
    }
}

impl From<Option<LoxError>> for LoxError {
    fn from(error: Option<LoxError>) -> Self {
        match error {
            Some(error) => error,
            None => LoxError::UnknownError,
        }
    }
}

impl LoxError {
    pub fn push(self, other: LoxError) -> LoxError {
        match self {
            LoxError::Errors(mut errors) => {
                errors.push(other);
                LoxError::Errors(errors)
            }
            _ => LoxError::Errors(vec![self, other]),
        }
    }
}