use std::iter::{Iterator, Peekable};
use std::str::Chars;
use regex::Regex;

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
    OpenParen,  // (
    CloseParen, // )
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
            "~"=> TokenKind::Tilde,
            "*" => TokenKind::Mul,
            "<" => TokenKind::Less,
            "=" => TokenKind::Equal,
            "." => TokenKind::Dot,
            "@" => TokenKind::At,
            ";" => TokenKind::SemiColon,
            "(" => TokenKind::OpenParen,
            ")" => TokenKind::CloseParen,
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
                    TokenKind::Str(symbol.get(1..(symbol.len()-1)).unwrap().to_string())
                } else {
                    match symbol.parse::<i32>() {
                        Ok(n) => TokenKind::IntConst(n),
                        Err(_) => {
                            let id_re = Regex::new(r"[[:alpha:]][[:alnum:]_]*").unwrap();
                            if id_re.is_match(&symbol) {
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

pub struct Token {
    pub token: TokenKind,
    pub line: usize,
    pub col: usize,
}

#[derive(PartialEq, Eq)]
enum State {
    Init,
    Number,
    Id,
    Str,
    Comment,
    Finish,
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

    fn token(&mut self, tok: String) -> Option<Token> {
        if tok.is_empty() {
            None
        } else {
            let tok_col = self.tok_col;
            self.tok_col = self.col;
            Some(Token {
                token: TokenKind::from(tok),
                line: self.line,
                col: tok_col,
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
                    Some(c2) if c2.is_digit(10) => State::Number,
                    _ => State::Finish,
                },
                '=' => match self.peek() {
                    Some('>') => {
                        tok.push(self.next().unwrap());
                        State::Finish
                    },
                    _ => State::Finish,
                },
                '<' => match self.peek() {
                    Some('-') | Some('=') => {
                        tok.push(self.next().unwrap());
                        State::Finish
                    },
                    _ => State::Finish,
                },
                '(' => match self.peek() {
                    Some('*') => {
                        self.next();
                        self.inner_comments = 1;
                        tok.pop();
                        State::Comment
                    },
                    _ => State::Finish,
                },
                '0'..='9' => match self.peek() {
                        Some(c2) if c2.is_digit(10) => State::Number,
                        _ => State::Finish,
                },
                'a'..='z' | 'A'..='Z' => match self.peek() {
                    Some(c2) if c2.is_digit(10) || c2.is_ascii_alphabetic() => State::Id,
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
            Some(c2) if c2.is_digit(10) || c2.is_ascii_alphabetic() => State::Id,
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
                },
                _ => State::Comment,
            },
            _ => State::Comment,
        }
    }

    fn str_(&mut self, ch: char, tok: &mut String) -> State {
        match ch {
            '\\' => {
                match self.peek() {
                    Some('n') => {
                        self.next();
                        tok.push('\n');
                        State::Str
                    },
                    Some('t') => {
                        self.next();
                        tok.push('\t');
                        State::Str
                    },
                    Some('f') => {
                        self.next();
                        tok.push('\x0c');
                        State::Str
                    },
                    Some('b') => {
                        self.next();
                        tok.push('\x08');
                        State::Str
                    },
                    Some('\0') => {
                        self.next();
                        *tok = "String contains escaped null character".to_string();
                        State::Finish
                    }
                    _ => {
                        tok.push(ch);
                        State::Str
                    }
                }
            }
            '"' => {
                tok.push(ch);
                State::Finish
            },
            '\n' => {
                self.next();
                *tok = "Unterminated string constant".to_string();
                State::Finish
            }
            '\0' => {
                self.next();
                *tok = "String contains null character".to_string();
                State::Finish
            },
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
                    self.update_pos(c);
                    state = match state {
                        State::Init => self.init(c, &mut tok),
                        State::Number => self.number(c, &mut tok),
                        State::Id => self.id(c, &mut tok),
                        State::Comment => self.comment(c),
                        State::Str => self.str_(c, &mut tok),
                        State::Finish => break,
                    }
                },
                None => break,
            }
        }

        let tok = self.token(tok);
        self.tok_col = self.col;
        tok
    }
}
