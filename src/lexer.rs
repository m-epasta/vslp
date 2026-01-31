#![allow(dead_code)]

use std::fmt;
use text_size::{TextRange, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Literals
    Identifier,
    String,
    Number,
    True,
    False,
    Nil,

    // Keywords
    Fn,
    Class,
    Let,
    If,
    Else,
    While,
    For,
    Return,
    This,
    Super,
    Import,
    Export,
    Async,
    Await,
    Try,
    Catch,
    Match,
    Struct,
    Enum,
    And,
    Or,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    PlusPlus,
    MinusMinus,
    AmpersandAmpersand,
    PipePipe,
    FatArrow,

    // Punctuation
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Colon,
    At,
    AtBracket,

    // String interpolation
    StringInterpStart,
    StringInterpMiddle,
    StringInterpEnd,
    DollarLeftBrace,

    // Special
    Whitespace,
    Comment,
    Newline,
    Error,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub range: TextRange,
}

impl Token {
    pub fn new(kind: TokenKind, text: String, range: TextRange) -> Self {
        Self { kind, text, range }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Nil => write!(f, "nil"),
            TokenKind::Fn => write!(f, "fn"),
            TokenKind::Class => write!(f, "class"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::While => write!(f, "while"),
            TokenKind::For => write!(f, "for"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::This => write!(f, "this"),
            TokenKind::Super => write!(f, "super"),
            TokenKind::Import => write!(f, "import"),
            TokenKind::Export => write!(f, "export"),
            TokenKind::Async => write!(f, "async"),
            TokenKind::Await => write!(f, "await"),
            TokenKind::Try => write!(f, "try"),
            TokenKind::Catch => write!(f, "catch"),
            TokenKind::Match => write!(f, "match"),
            TokenKind::Struct => write!(f, "struct"),
            TokenKind::Enum => write!(f, "enum"),
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            _ => write!(f, "{:?}", self),
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: std::str::Chars<'a>,
    current_char: Option<char>,
    position: TextSize,
    start_position: TextSize,
    interp_stack: Vec<usize>,
    brace_depth: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.chars();
        let current_char = chars.next();

        Self {
            input,
            chars,
            current_char,
            position: TextSize::new(0),
            start_position: TextSize::new(0),
            interp_stack: Vec::new(),
            brace_depth: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        while !self.is_at_end() {
            self.start_position = self.position;
            if let Some(token) = self.next_token() {
                if token.kind == TokenKind::Newline {
                    if let Some(last_real) = tokens
                        .iter()
                        .rev()
                        .find(|t| !matches!(t.kind, TokenKind::Whitespace | TokenKind::Comment))
                    {
                        if matches!(
                            last_real.kind,
                            TokenKind::Identifier
                                | TokenKind::Number
                                | TokenKind::String
                                | TokenKind::RightParen
                                | TokenKind::RightBrace
                                | TokenKind::RightBracket
                                | TokenKind::True
                                | TokenKind::False
                                | TokenKind::Nil
                                | TokenKind::Return
                                | TokenKind::Let
                        ) {
                            tokens.push(Token::new(
                                TokenKind::Semicolon,
                                ";".to_string(),
                                token.range,
                            ));
                        }
                    }
                }
                tokens.push(token);
            }
        }

        tokens.push(Token::new(
            TokenKind::Eof,
            String::new(),
            TextRange::new(self.position, self.position),
        ));

        tokens
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.current_char? {
            '\n' => self.newline(),
            c if c.is_whitespace() => self.whitespace(),
            '/' => {
                if self.peek() == Some('/') {
                    self.line_comment()
                } else if self.peek() == Some('*') {
                    self.block_comment()
                } else {
                    Some(self.single_char_token(TokenKind::Slash))
                }
            }
            '(' => Some(self.single_char_token(TokenKind::LeftParen)),
            ')' => Some(self.single_char_token(TokenKind::RightParen)),
            '{' => {
                self.brace_depth += 1;
                Some(self.single_char_token(TokenKind::LeftBrace))
            }
            '}' => {
                if let Some(&start_depth) = self.interp_stack.last() {
                    if self.brace_depth == start_depth {
                        self.advance(); // skip '}'
                        self.interp_stack.pop();
                        return self.string();
                    }
                }
                if self.brace_depth > 0 {
                    self.brace_depth -= 1;
                }
                Some(self.single_char_token(TokenKind::RightBrace))
            }
            '[' => Some(self.single_char_token(TokenKind::LeftBracket)),
            ']' => Some(self.single_char_token(TokenKind::RightBracket)),
            ',' => Some(self.single_char_token(TokenKind::Comma)),
            '.' => Some(self.single_char_token(TokenKind::Dot)),
            ';' => Some(self.single_char_token(TokenKind::Semicolon)),
            ':' => Some(self.single_char_token(TokenKind::Colon)),
            '@' => {
                if self.peek() == Some('[') {
                    self.advance();
                    self.advance();
                    Some(self.make_token(TokenKind::AtBracket))
                } else {
                    Some(self.single_char_token(TokenKind::At))
                }
            }
            '+' => {
                if self.peek() == Some('+') {
                    self.advance();
                    self.advance();
                    Some(self.make_token(TokenKind::PlusPlus))
                } else {
                    Some(self.single_char_token(TokenKind::Plus))
                }
            }
            '-' => {
                if self.peek() == Some('-') {
                    self.advance();
                    self.advance();
                    Some(self.make_token(TokenKind::MinusMinus))
                } else {
                    Some(self.single_char_token(TokenKind::Minus))
                }
            }
            '*' => Some(self.single_char_token(TokenKind::Star)),
            '%' => Some(self.single_char_token(TokenKind::Percent)),
            '!' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.advance();
                    Some(self.make_token(TokenKind::BangEqual))
                } else {
                    Some(self.single_char_token(TokenKind::Bang))
                }
            }
            '=' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.advance();
                    Some(self.make_token(TokenKind::EqualEqual))
                } else if self.peek() == Some('>') {
                    self.advance();
                    self.advance();
                    Some(self.make_token(TokenKind::FatArrow))
                } else {
                    Some(self.single_char_token(TokenKind::Equal))
                }
            }
            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.advance();
                    Some(self.make_token(TokenKind::LessEqual))
                } else {
                    Some(self.single_char_token(TokenKind::Less))
                }
            }
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    self.advance();
                    Some(self.make_token(TokenKind::GreaterEqual))
                } else {
                    Some(self.single_char_token(TokenKind::Greater))
                }
            }
            '&' => {
                if self.peek() == Some('&') {
                    self.advance();
                    self.advance();
                    Some(self.make_token(TokenKind::AmpersandAmpersand))
                } else {
                    Some(self.error_token("Unexpected character '&'"))
                }
            }
            '|' => {
                if self.peek() == Some('|') {
                    self.advance();
                    self.advance();
                    Some(self.make_token(TokenKind::PipePipe))
                } else {
                    Some(self.error_token("Unexpected character '|'"))
                }
            }
            '"' => self.string(),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_alphabetic() || c == '_' => self.identifier(),
            c => Some(self.error_token(&format!("Unexpected character '{}'", c))),
        }
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(ch) = self.current_char {
            self.position += TextSize::of(ch);
            self.current_char = self.chars.next();
            Some(ch)
        } else {
            None
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.as_str().chars().next()
    }

    fn peek_ahead(&self, n: usize) -> Option<char> {
        self.chars.as_str().chars().nth(n)
    }

    fn is_at_end(&self) -> bool {
        self.current_char.is_none()
    }

    fn single_char_token(&mut self, kind: TokenKind) -> Token {
        self.advance();
        self.make_token(kind)
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        let range = TextRange::new(self.start_position, self.position);
        let text = self.input[range].to_string();
        Token::new(kind, text, range)
    }

    fn error_token(&mut self, message: &str) -> Token {
        self.advance();
        Token::new(
            TokenKind::Error,
            message.to_string(),
            TextRange::new(self.start_position, self.position),
        )
    }

    fn whitespace(&mut self) -> Option<Token> {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() && ch != '\n' {
                self.advance();
            } else {
                break;
            }
        }
        Some(self.make_token(TokenKind::Whitespace))
    }

    fn newline(&mut self) -> Option<Token> {
        self.advance();
        Some(self.make_token(TokenKind::Newline))
    }

    fn line_comment(&mut self) -> Option<Token> {
        // Skip //
        self.advance();
        self.advance();

        while let Some(ch) = self.current_char {
            if ch == '\n' {
                break;
            }
            self.advance();
        }

        Some(self.make_token(TokenKind::Comment))
    }

    fn block_comment(&mut self) -> Option<Token> {
        // Skip /*
        self.advance();
        self.advance();

        while let Some(ch) = self.current_char {
            if ch == '*' && self.peek() == Some('/') {
                self.advance(); // *
                self.advance(); // /
                break;
            }
            self.advance();
        }

        Some(self.make_token(TokenKind::Comment))
    }

    fn string(&mut self) -> Option<Token> {
        // Only skip opening quote if we are not continuing from an interpolation
        let is_continuation = !self.interp_stack.is_empty();
        if !is_continuation && self.current_char == Some('"') {
            self.advance();
        }

        while let Some(ch) = self.current_char {
            if ch == '"' {
                self.advance();
                if self.interp_stack.is_empty() {
                    return Some(self.make_token(TokenKind::String));
                } else {
                    // This case shouldn't happen with correct nesting, but for safety:
                    return Some(self.make_token(TokenKind::String));
                }
            }

            if ch == '$' && self.peek() == Some('{') {
                let token_kind = if is_continuation {
                    TokenKind::StringInterpMiddle
                } else {
                    TokenKind::StringInterpStart
                };
                let token = self.make_token(token_kind);
                self.advance(); // $
                self.advance(); // {
                self.interp_stack.push(self.brace_depth);
                return Some(token);
            }

            if ch == '\\' {
                self.advance(); // Skip escape character
                if self.current_char.is_some() {
                    self.advance(); // Skip escaped character
                }
            } else {
                self.advance();
            }
        }

        if !is_continuation && self.current_char == Some('"') {
            self.advance();
            Some(self.make_token(TokenKind::String))
        } else if is_continuation && self.is_at_end() {
            // Maybe we should return StringInterpEnd if it ended?
            // But usually it ends with '"'
            Some(self.error_token("Unterminated interpolated string"))
        } else if self.current_char == Some('"') {
            self.advance();
            Some(self.make_token(TokenKind::StringInterpEnd))
        } else {
            Some(self.error_token("Unterminated string"))
        }
    }

    fn number(&mut self) -> Option<Token> {
        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        // Handle decimal point
        if self.current_char == Some('.') && self.peek().map_or(false, |ch| ch.is_ascii_digit()) {
            self.advance(); // .
            while let Some(ch) = self.current_char {
                if ch.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        Some(self.make_token(TokenKind::Number))
    }

    fn identifier(&mut self) -> Option<Token> {
        while let Some(ch) = self.current_char {
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let text = &self.input[TextRange::new(self.start_position, self.position)];
        let kind = match text {
            "fn" => TokenKind::Fn,
            "class" => TokenKind::Class,
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "return" => TokenKind::Return,
            "this" => TokenKind::This,
            "super" => TokenKind::Super,
            "import" => TokenKind::Import,
            "export" => TokenKind::Export,
            "async" => TokenKind::Async,
            "await" => TokenKind::Await,
            "try" => TokenKind::Try,
            "catch" => TokenKind::Catch,
            "match" => TokenKind::Match,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "nil" => TokenKind::Nil,
            _ => TokenKind::Identifier,
        };

        Some(self.make_token(kind))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let input = "fn class let if else while for return";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].kind, TokenKind::Fn);
        assert_eq!(tokens[2].kind, TokenKind::Class);
        assert_eq!(tokens[4].kind, TokenKind::Let);
    }

    #[test]
    fn test_operators() {
        let input = "+ - * / == != <= >= ++ --";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].kind, TokenKind::Plus);
        assert_eq!(tokens[2].kind, TokenKind::Minus);
        assert_eq!(tokens[8].kind, TokenKind::EqualEqual);
    }

    #[test]
    fn test_string() {
        let input = r#""hello world""#;
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].kind, TokenKind::String);
        assert_eq!(tokens[0].text, r#""hello world""#);
    }

    #[test]
    fn test_number() {
        let input = "123 45.67";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].kind, TokenKind::Number);
        assert_eq!(tokens[0].text, "123");
        assert_eq!(tokens[2].kind, TokenKind::Number);
        assert_eq!(tokens[2].text, "45.67");
    }
}
