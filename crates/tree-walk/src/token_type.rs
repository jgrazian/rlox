use std::cmp::Eq;
use std::hash::{Hash, Hasher};
use std::mem;

#[derive(Debug, Clone)]
pub enum TokenType {
    // Single Characters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two characters
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // EOF
    Eof,
}

impl Hash for TokenType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            // This is a hack
            Self::Number(n) => integer_decode(*n).hash(state),
            Self::Identifier(s) => s.hash(state),
            Self::String(s) => s.hash(state),
            _ => mem::discriminant(self).hash(state),
        }
    }
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Number(n) => match other {
                Self::Number(n2) => n == n2,
                _ => false,
            },
            Self::Identifier(s) => match other {
                Self::Identifier(s2) => s == s2,
                _ => false,
            },
            Self::String(s) => match other {
                Self::String(s2) => s == s2,
                _ => false,
            },
            _ => mem::discriminant(self) == mem::discriminant(other),
        }
    }
}
impl Eq for TokenType {}

pub fn integer_decode(val: f64) -> (u64, i16, i8) {
    let bits: u64 = unsafe { mem::transmute(val) };
    let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
    let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
    let mantissa = if exponent == 0 {
        (bits & 0xfffffffffffff) << 1
    } else {
        (bits & 0xfffffffffffff) | 0x10000000000000
    };

    exponent -= 1023 + 52;
    (mantissa, exponent, sign)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::hash_map::DefaultHasher;

    #[test]
    fn test_hash() {
        assert_eq!(
            {
                let mut hasher = DefaultHasher::new();
                TokenType::Number(42.0).hash(&mut hasher);
                hasher.finish()
            },
            {
                let mut hasher = DefaultHasher::new();
                integer_decode(42.0).hash(&mut hasher);
                hasher.finish()
            }
        );

        assert_eq!(
            {
                let mut hasher = DefaultHasher::new();
                TokenType::Identifier("Hello World!".to_string()).hash(&mut hasher);
                hasher.finish()
            },
            {
                let mut hasher = DefaultHasher::new();
                "Hello World!".to_string().hash(&mut hasher);
                hasher.finish()
            }
        );

        assert_eq!(
            {
                let mut hasher = DefaultHasher::new();
                TokenType::String("Hello World!".to_string()).hash(&mut hasher);
                hasher.finish()
            },
            {
                let mut hasher = DefaultHasher::new();
                "Hello World!".to_string().hash(&mut hasher);
                hasher.finish()
            }
        );

        assert_eq!(
            {
                let mut hasher = DefaultHasher::new();
                TokenType::Bang.hash(&mut hasher);
                hasher.finish()
            },
            {
                let mut hasher = DefaultHasher::new();
                mem::discriminant(&TokenType::Bang).hash(&mut hasher);
                hasher.finish()
            }
        );
    }

    #[test]
    fn test_partial_eq() {
        assert_eq!(TokenType::Number(42.0), TokenType::Number(42.0));
        assert_ne!(TokenType::Number(42.0), TokenType::Number(0.0));
        assert_ne!(TokenType::Number(42.0), TokenType::Bang);

        assert_eq!(
            TokenType::Identifier("Hello World".to_string()),
            TokenType::Identifier("Hello World".to_string())
        );
        assert_ne!(
            TokenType::Identifier("Hello World".to_string()),
            TokenType::Identifier("".to_string())
        );
        assert_ne!(
            TokenType::Identifier("Hello World".to_string()),
            TokenType::Bang
        );

        assert_eq!(
            TokenType::String("Hello World".to_string()),
            TokenType::String("Hello World".to_string())
        );
        assert_ne!(
            TokenType::String("Hello World".to_string()),
            TokenType::String("".to_string())
        );
        assert_ne!(
            TokenType::String("Hello World".to_string()),
            TokenType::Identifier("".to_string())
        );

        assert_eq!(TokenType::Bang, TokenType::Bang);
        assert_ne!(TokenType::Bang, TokenType::Dot);
    }
}
