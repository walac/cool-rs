use regex::Regex;
use std::fmt;
use std::iter::{Iterator, Peekable};
use std::str::Chars;

lazy_static! {
    static ref ID_MATCH: Regex = Regex::new(r"[[:alpha:]][[:alnum:]_]*").unwrap();
}

pub enum TokenKind {
    Darrow,     // =>
    Assign,     // <-
    LessEqual,  // <=
    Class,      // class
    If,         // if
    Else,       // else
    Fi,         // fi
    In,         // in
    Inherits,   // inherits
    Let,        // let
    Loop,       // loop
    Pool,       // pool
    Then,       // then
    While,      // while
    Case,       // case
    Esac,       // esac
    Of,         // of
    New,        // new
    IsVoid,     // isvoid
    Not,        // not
    Plus,       // +
    Div,        // /
    Tilde,      // ~
    Mul,        // *
    Less,       // <
    Equal,      // =
    Dot,        // .
    At,         // @
    SemiColon,  // ;
    Colon,      // :
    OpenParen,  // (
    CloseParen, // )
    OpenBrace,  // {
    CloseBrace, // }
    Comma,      // ,
    Minus,      // -
    BoolConst(bool),
    Str(String),
    IntConst(i32),
    Type(String),
    Object(String),
    Err(String),
}

impl From<String> for TokenKind {
    fn from(symbol: String) -> TokenKind {
        let low_case = symbol.to_ascii_lowercase();
        match low_case.as_str() {
            "=>" => TokenKind::Darrow,
            "<-" => TokenKind::Assign,
            "<=" => TokenKind::LessEqual,
            "+" => TokenKind::Plus,
            "/" => TokenKind::Div,
            "~" => TokenKind::Tilde,
            "*" => TokenKind::Mul,
            "<" => TokenKind::Less,
            "=" => TokenKind::Equal,
            "." => TokenKind::Dot,
            "@" => TokenKind::At,
            ";" => TokenKind::SemiColon,
            ":" => TokenKind::Colon,
            "(" => TokenKind::OpenParen,
            ")" => TokenKind::CloseParen,
            "{" => TokenKind::OpenBrace,
            "}" => TokenKind::CloseBrace,
            "," => TokenKind::Comma,
            "-" => TokenKind::Minus,
            "class" => TokenKind::Class,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "fi" => TokenKind::Fi,
            "in" => TokenKind::In,
            "inherits" => TokenKind::Inherits,
            "let" => TokenKind::Let,
            "loop" => TokenKind::Loop,
            "pool" => TokenKind::Pool,
            "then" => TokenKind::Then,
            "while" => TokenKind::While,
            "case" => TokenKind::Case,
            "esac" => TokenKind::Esac,
            "of" => TokenKind::Of,
            "new" => TokenKind::New,
            "isvoid" => TokenKind::IsVoid,
            "not" => TokenKind::Not,
            "" => TokenKind::Err(symbol),
            _ => {
                if symbol.starts_with('t') && low_case == "true" {
                    TokenKind::BoolConst(true)
                } else if symbol.starts_with('f') && low_case == "false" {
                    TokenKind::BoolConst(false)
                } else if symbol.starts_with('"') && symbol.ends_with('"') {
                    TokenKind::Str(symbol.get(1..(symbol.len() - 1)).unwrap().to_string())
                } else {
                    match symbol.parse::<i32>() {
                        Ok(n) => TokenKind::IntConst(n),
                        Err(_) => {
                            if ID_MATCH.is_match(&symbol) {
                                if symbol.chars().nth(0).unwrap().is_ascii_uppercase() {
                                    TokenKind::Type(symbol)
                                } else {
                                    TokenKind::Object(symbol)
                                }
                            } else {
                                TokenKind::Err(symbol)
                            }
                        }
                    }
                }
            }
        }
    }
}

impl fmt::Debug for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenKind::Darrow => f.write_str("DARROW"),
            TokenKind::Assign => f.write_str("ASSIGN"),
            TokenKind::LessEqual => f.write_str("LE"),
            TokenKind::Class => f.write_str("CLASS"),
            TokenKind::If => f.write_str("IF"),
            TokenKind::Else => f.write_str("ELSE"),
            TokenKind::Fi => f.write_str("FI"),
            TokenKind::In => f.write_str("IN"),
            TokenKind::Inherits => f.write_str("INHERITS"),
            TokenKind::Let => f.write_str("LET"),
            TokenKind::Loop => f.write_str("LOOP"),
            TokenKind::Pool => f.write_str("POOL"),
            TokenKind::Then => f.write_str("THEN"),
            TokenKind::While => f.write_str("WHILE"),
            TokenKind::Case => f.write_str("CASE"),
            TokenKind::Esac => f.write_str("ESAC"),
            TokenKind::Of => f.write_str("OF"),
            TokenKind::New => f.write_str("NEW"),
            TokenKind::IsVoid => f.write_str("ISVOID"),
            TokenKind::Not => f.write_str("NOT"),
            TokenKind::Plus => f.write_str("'+'"),
            TokenKind::Div => f.write_str("'/'"),
            TokenKind::Tilde => f.write_str("'~'"),
            TokenKind::Mul => f.write_str("'*'"),
            TokenKind::Less => f.write_str("'<'"),
            TokenKind::Equal => f.write_str("'='"),
            TokenKind::Dot => f.write_str("'.'"),
            TokenKind::At => f.write_str("'@'"),
            TokenKind::SemiColon => f.write_str("';'"),
            TokenKind::Colon => f.write_str("':'"),
            TokenKind::OpenParen => f.write_str("'('"),
            TokenKind::CloseParen => f.write_str("')'"),
            TokenKind::OpenBrace => f.write_str("'{'"),
            TokenKind::CloseBrace => f.write_str("'}'"),
            TokenKind::Comma => f.write_str("','"),
            TokenKind::Minus => f.write_str("'-'"),
            TokenKind::BoolConst(b) => f.write_fmt(format_args!("BOOL_CONST {}", b)),
            TokenKind::Str(ref s) => f.write_fmt(format_args!(r#"STR_CONST "{}""#, s)),
            TokenKind::IntConst(i) => f.write_fmt(format_args!("INT_CONST {}", i)),
            TokenKind::Type(ref s) => f.write_fmt(format_args!("TYPEID {}", s)),
            TokenKind::Object(ref s) => f.write_fmt(format_args!("OBJECTID {}", s)),
            TokenKind::Err(ref s) => f.write_fmt(format_args!("ERROR {}", s)),
        }
    }
}

pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
}

#[derive(PartialEq, Eq, Debug)]
enum State {
    Init,
    Number,
    Id,
    Str,
    Comment,
    LineComment,
    Finish,
    Error,
}

pub struct Tokenizer<'a> {
    iter: Peekable<Chars<'a>>,
    line: usize,
    tok_col: usize,
    col: usize,
    inner_comments: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(src: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            iter: src.chars().peekable(),
            line: 1,
            tok_col: 1,
            col: 1,
            inner_comments: 0,
        }
    }

    fn next(&mut self) -> Option<char> {
        self.iter.next()
    }

    fn peek(&mut self) -> Option<char> {
        self.iter.peek().map(|&c| c)
    }

    fn update_pos(&mut self, ch: char) {
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
            self.tok_col = 1;
        } else {
            self.col += 1;
        }
    }

    fn token(&mut self, tok: String, is_err: bool) -> Option<Token> {
        if tok.is_empty() {
            None
        } else if is_err {
            Some(Token {
                kind: TokenKind::Err(tok),
                line: self.line,
                col: self.tok_col,
            })
        } else {
            Some(Token {
                kind: TokenKind::from(tok),
                line: self.line,
                col: self.tok_col,
            })
        }
    }
}

impl<'a> Tokenizer<'a> {
    fn init(&mut self, ch: char, tok: &mut String) -> State {
        if ch.is_ascii_whitespace() {
            self.tok_col = self.col;
            State::Init
        } else {
            tok.push(ch);
            match ch {
                '-' => match self.peek() {
                    Some('-') => {
                        tok.clear();
                        self.next();
                        State::LineComment
                    }
                    _ => State::Finish,
                },
                '=' => match self.peek() {
                    Some('>') => {
                        tok.push(self.next().unwrap());
                        State::Finish
                    }
                    _ => State::Finish,
                },
                '<' => match self.peek() {
                    Some('-') | Some('=') => {
                        tok.push(self.next().unwrap());
                        State::Finish
                    }
                    _ => State::Finish,
                },
                '(' => match self.peek() {
                    Some('*') => {
                        self.next();
                        self.inner_comments = 1;
                        tok.pop();
                        State::Comment
                    }
                    _ => State::Finish,
                },
                '0'..='9' => match self.peek() {
                    Some(c2) if c2.is_digit(10) => State::Number,
                    _ => State::Finish,
                },
                'a'..='z' | 'A'..='Z' => match self.peek() {
                    Some(c2) if c2.is_digit(10) || c2.is_ascii_alphabetic() || c2 == '_' => {
                        State::Id
                    }
                    _ => State::Finish,
                },
                '"' => State::Str,
                _ => State::Finish,
            }
        }
    }

    fn number(&mut self, ch: char, tok: &mut String) -> State {
        tok.push(ch);
        match self.peek() {
            Some(c) if c.is_digit(10) => State::Number,
            _ => State::Finish,
        }
    }

    fn id(&mut self, ch: char, tok: &mut String) -> State {
        tok.push(ch);
        match self.peek() {
            Some(c2) if c2.is_digit(10) || c2.is_ascii_alphabetic() || c2 == '_' => State::Id,
            _ => State::Finish,
        }
    }

    fn comment(&mut self, ch: char) -> State {
        match ch {
            '*' => match self.peek() {
                None => State::Comment,
                Some(')') => {
                    self.next();
                    self.inner_comments -= 1;
                    if self.inner_comments == 0 {
                        State::Init
                    } else {
                        State::Comment
                    }
                }
                _ => State::Comment,
            },
            '(' => match self.peek() {
                None => State::Comment,
                Some('*') => {
                    self.next();
                    self.inner_comments += 1;
                    State::Comment
                }
                _ => State::Comment,
            },
            _ => State::Comment,
        }
    }

    fn str_(&mut self, ch: char, tok: &mut String) -> State {
        match ch {
            '\\' => match self.peek() {
                Some(c) if r#"ntfb\""#.contains(c) => {
                    self.next();
                    tok.push('\\');
                    tok.push(c);
                    State::Str
                }
                Some('\0') => {
                    self.next();
                    *tok = "String contains escaped null character".to_string();
                    State::Finish
                }
                Some('\n') => {
                    self.next();
                    self.update_pos('\n');
                    tok.push_str(r"\n");
                    State::Str
                }
                Some(c) => {
                    self.next();
                    tok.push(c);
                    State::Str
                }
                None => State::Finish,
            },
            '"' => {
                tok.push(ch);
                State::Finish
            }
            '\n' => {
                self.next();
                eprintln!("Str Error({}): \"{}\"", self.line, tok);
                *tok = "\"Unterminated string constant\"".to_string();
                State::Error
            }
            '\0' => {
                self.next();
                *tok = "\"String contains null character\"".to_string();
                State::Error
            }
            _ => {
                tok.push(ch);
                State::Str
            }
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let mut tok = String::with_capacity(32);
        let mut state = State::Init;

        loop {
            let ch = self.next();

            match ch {
                Some(c) => {
                    state = match state {
                        State::Init => self.init(c, &mut tok),
                        State::Number => self.number(c, &mut tok),
                        State::Id => self.id(c, &mut tok),
                        State::Comment => self.comment(c),
                        State::Str => self.str_(c, &mut tok),
                        State::LineComment => match c {
                            '\n' => State::Init,
                            _ => State::LineComment,
                        },
                        State::Finish => State::Finish,
                        State::Error => State::Error,
                    };
                    self.update_pos(c);
                }
                None => state = State::Finish,
            }

            if state == State::Finish || state == State::Error {
                let tok = self.token(tok, state == State::Error);
                self.tok_col = self.col;
                break tok;
            }
        }
    }
}
