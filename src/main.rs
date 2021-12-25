#![allow(dead_code)]

extern crate clap;

use clap::{App, Arg};
use lex::Lexer;
use std::fs;

fn main() {
    let matches = App::new("Rust 8cc")
        .version("0.1")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets input file")
                .required(true)
                .index(1),
        )
        .get_matches();

    println!("Using input file: {}", matches.value_of("INPUT").unwrap());

    let file_path = matches.value_of("INPUT").unwrap();

    let mut lexer = Lexer::new(file_path.to_string());

    for i in 0..20 {
        let token = lexer.read_token();
        if let Some(token) = token {
            dbg!(token.kind);
        }
    }
}

mod lex {
    use std::fs;

    #[derive(Debug, Clone)]
    pub struct Pos {
        line: usize,
        column: usize,
    }

    impl Pos {
        pub fn start() -> Self {
            Pos { line: 1, column: 0 }
        }
        pub fn newline(&self) -> Self {
            Pos {
                line: self.line + 1,
                column: 0,
            }
        }
        pub fn next(&self) -> Self {
            Pos {
                line: self.line,
                column: self.column + 1,
            }
        }

        pub fn previous(&self, ch: char) -> Self {
            // TODO The position here is incorrect for new line. But we will get the correct
            // position as soon as we read next char. So for now it should only be called from
            // unread
            if ch == '\n' {
                Pos {
                    line: self.line - 1,
                    column: 1,
                }
            } else {
                Pos {
                    line: self.line,
                    column: self.column - 1,
                }
            }
        }
    }

    pub struct File {
        pub name: String,
        pub text: Vec<char>,
        pub current_pos: Pos,
        pub tokens: usize,
        pub last: Option<char>,
        pub index: usize,
    }

    impl File {
        fn new(name: String) -> File {
            let text = fs::read_to_string(&name).unwrap();
            File {
                name: name,
                text: text.chars().collect(),
                current_pos: Pos::start(),
                tokens: 0,
                last: None,
                index: 0,
            }
        }
    }

    #[derive(Debug)]
    pub enum Keyword {
        Semi,
        LBrace,
        RBrace,
        LParen,
        RParen,
        LBracket,
        RBracket,
        OpAdd,
        OpInc,
        OpAddEq,
    }

    impl Keyword {
        fn to_string(&self) -> String {
            use Keyword::*;
            let p = match self {
                OpAdd => "+",
                OpInc => "++",
                OpAddEq => "+=",
                LBrace => "{",
                RBrace => "}",
                LBracket => "[",
                RBracket => "]",
                LParen => "(",
                RParen => ")",
                Semi => ";",
            };

            p.to_string()
        }

        fn from(keyword: &str) -> Self {
            use Keyword::*;
            match keyword {
                "+" => OpAdd,
                "++" => OpInc,
                "+=" => OpAddEq,
                "(" => LParen,
                ")" => RParen,
                "{" => LBrace,
                "}" => RBrace,
                "[" => LBracket,
                "]" => RBracket,
                ";" => Semi,
                _ => panic!("Unrecognized keyword {}", keyword),
            }
        }
    }

    #[derive(Debug)]
    pub enum TokenKind {
        Ident(String),
        Keyword(Keyword),
        Number(String),
        Char(char),
        String(String),
        Eof,
        Invalid,
        MinCppToken,
        Newline,
        Space,
        MacroParameter { is_vararg: bool, postion: usize },
    }

    #[derive(Debug)]
    pub struct Token {
        pub kind: TokenKind,
        // TODO is this needed???
        // Reference to file pointer
        pub pos: Pos,
        pub space: bool,
        pub bol: bool,
        pub count: usize,
        // TODO hideset Option<Set>
    }

    pub struct Lexer {
        pub files: Vec<File>,
        pub index: usize,
        pub end: bool,
        pub count: usize,
    }

    impl Lexer {
        pub fn new(name: String) -> Self {
            let file = File::new(name);
            Lexer {
                files: vec![file],
                index: 0,
                end: false,
                count: 0,
            }
        }

        pub fn read_char(&mut self) -> Option<char> {
            if self.end {
                return None;
            }

            let file = &mut self.files[self.index];

            let ch = file.text[file.index];
            if file.index + 1 < file.text.len() {
                file.index += 1;
                file.current_pos = if ch == '\n' {
                    file.current_pos.newline()
                } else {
                    file.current_pos.next()
                };
            } else if self.index + 1 < self.files.len() {
                self.index += 1;
            } else {
                self.end = true;
            }

            Some(ch)
        }
        pub fn unread(&mut self, ch: char) {
            let file = &mut self.files[self.index];
            file.index -= 1;

            file.current_pos = file.current_pos.previous(ch);
        }

        pub fn read_token(&mut self) -> Option<Token> {
            let ch = self.read_char()?;
            let file = &mut self.files[self.index];
            let pos = file.current_pos.clone();

            // TODO Are \f and \v also handled here
            if ch.is_whitespace() && ch != '\n' {
                return Some(self.read_space_token(pos));
            }

            match ch {
                '\n' => Some(self.make_newline_token()),
                '{' | '}' | '[' | ']' | '(' | ')' | '?' | ',' | '~' | ';' => {
                    Some(self.make_keyword_token(Keyword::from(&ch.to_string())))
                }
                'a'..='t' | 'v'..='z' | 'A'..='K' | 'M'..='T' | 'V'..='Z' => {
                    Some(self.read_ident(ch, pos))
                }
                '0'..='9' => Some(self.read_number(ch, pos)),
                _ => panic!("Unrecognized character token {}", ch),
            }
        }

        pub fn read_number(&mut self, ch: char, pos: Pos) -> Token {
            let mut number_chars = vec![ch];
            let mut last = ch;
            loop {
                let ch = self.read_char();
                if ch == None {
                    break;
                }
                let ch = ch.unwrap();

                let floatnumber = (last == 'e' || last == 'E' || last == 'p' || last == 'P')
                    && (ch == '+' || ch == '-');

                if !ch.is_ascii_digit() && !ch.is_ascii_alphabetic() && ch != '.' && !floatnumber {
                    self.unread(ch);
                    break;
                }

                number_chars.push(ch);
            }

            let token_index = self.count;
            self.count += 1;
            Token {
                kind: TokenKind::Number(number_chars.into_iter().collect()),
                bol: pos.column == 1,
                pos: pos,
                space: false,
                count: token_index,
            }
        }

        pub fn read_ident(&mut self, ch: char, pos: Pos) -> Token {
            let mut ident_chars = vec![ch];

            loop {
                let ch = self.read_char();
                if ch == None {
                    break;
                }

                let ch = ch.unwrap();
                if ch.is_alphanumeric() || ch == '$' || ch == '_' {
                    ident_chars.push(ch);
                // TODO handle \u and \U  unicode characters
                } else {
                    self.unread(ch);
                    break;
                }
            }

            let token_index = self.count;
            self.count += 1;
            Token {
                kind: TokenKind::Ident(ident_chars.into_iter().collect()),
                bol: pos.column == 1,
                pos: pos,
                space: false,
                count: token_index,
            }
        }

        pub fn read_space_token(&mut self, pos: Pos) -> Token {
            loop {
                let ch = self.read_char();
                if ch == None {
                    break;
                }

                let ch = ch.unwrap();
                if ch.is_whitespace() {
                    continue;
                } else {
                    self.unread(ch);
                    break;
                }
            }

            self.make_space_token(pos)
        }

        fn make_newline_token(&mut self) -> Token {
            let file = &mut self.files[self.index];

            let token_index = self.count;
            self.count += 1;
            Token {
                kind: TokenKind::Newline,
                pos: file.current_pos.clone(),
                space: false,
                bol: false,
                count: token_index,
            }
        }

        fn make_keyword_token(&mut self, keyword: Keyword) -> Token {
            let file = &mut self.files[self.index];

            let token_index = self.count;
            self.count += 1;

            Token {
                kind: TokenKind::Keyword(keyword),
                pos: file.current_pos.clone(),
                space: false,
                bol: false,
                count: token_index,
            }
        }

        fn make_space_token(&mut self, pos: Pos) -> Token {
            let file = &mut self.files[self.index];

            let token_index = self.count;
            self.count += 1;
            Token {
                kind: TokenKind::Space,
                pos: pos,
                space: true,
                bol: false,
                count: token_index,
            }
        }
    }
}
