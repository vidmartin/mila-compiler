
use std::io::Read;

use crate::tokens::Token;

#[derive(Clone, Copy)]
pub enum LexError {
    Unexpected(char),
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match *self {
            LexError::Unexpected(c) => format!("unexpected '{}'", c)
        })
    }
}

pub struct Lexer<S: Iterator<Item = char>> {
    curr_char: Option<char>,
    wrapped_stream: S,
    error: Option<LexError>,
}

impl<S: Iterator<Item = char>> Lexer<S> {
    pub fn new(mut stream: S) -> Self {
        Self {
            curr_char: stream.next(),
            wrapped_stream: stream,
            error: None,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.curr_char = self.wrapped_stream.next();
        return self.curr_char;
    }

    fn set_error(&mut self, error: LexError) {
        self.error = Some(error);
        self.curr_char = None;
    }

    fn unexpected(&mut self, c: char) {
        self.set_error(LexError::Unexpected(c))
    }

    pub fn get_error(&self) -> Option<LexError> {
        self.error
    }

    fn lex(&mut self) -> Option<Token> {
        while let Some(true) = self.curr_char.map(|c| c.is_whitespace()) {
            self.advance(); // skip whitespaces
        }

        let tok = match self.curr_char? {
            ':' => {
                match self.advance()? {
                    '=' => Token::TkAssign,
                    _ => { return Some(Token::TkColon); },
                }
            },
            ';' => Token::TkSemicolon,
            '[' => Token::TkSqOpen,
            ']' => Token::TkSqClose,
            '(' => Token::TkParOpen,
            ')' => Token::TkParClose,
            '.' => {
                match self.advance()? {
                    '.' => Token::TkDotDot,
                    _ => { return Some(Token::TkDot); },
                }
            },
            '<' => {
                match self.advance()? {
                    '>' => Token::TkNotEq,
                    '=' => Token::TkLessOrEq,
                    _ => { return Some(Token::TkLess); },
                }
            },
            '>' => {
                match self.advance()? {
                    '=' => Token::TkMoreOrEq,
                    _ => { return Some(Token::TkMore); },
                }
            },
            '=' => Token::TkEq,
            '+' => Token::TkAdd,
            '-' => Token::TkSub,
            '*' => Token::TkMul,
            c if c == '$' || c == '&' => {
                self.advance()?;
                return self.lex_num(if c == '$' { 16 } else { 8 });
            },
            c if c.is_digit(10) => { 
                return self.lex_num(10); 
            },
            c if c.is_alphabetic() || c == '_' => {
                todo!() // identifiers & keywords
            },
            c => {
                self.unexpected(c);
                None?
            }
        };

        // this will be executed unless an explicit return statement
        // was reached in the match statement above:
        self.advance();
        return Some(tok);
    }

    fn lex_num(&mut self, radix: u32) -> Option<Token> {
        let mut val: i64 = 0;
        while let Some(ch) = self.curr_char {
            if let Some(dig) = ch.to_digit(radix) {
                val *= radix as i64;
                val += dig as i64;
                self.advance();
            } else {
                break;
            }
        }
        return Some(Token::LitInt(val));
    }
}

impl<S: Iterator<Item = char>> Iterator for Lexer<S> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex()
    }
}
