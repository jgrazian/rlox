use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    String(String),
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{}", s),
        }
    }
}
