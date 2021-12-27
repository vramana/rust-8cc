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

    enum Encoding {
        None,
        WChar,
        Char16,
        Char32,
    }

    #[derive(Debug)]
    pub enum Keyword {
        Semi,
        Colon,
        LBrace,
        RBrace,
        LParen,
        RParen,
        LBracket,
        RBracket,

        OpAdd,
        OpInc,
        OpAddEq,

        OpSub,
        OpDec,
        OpSubEq,

        OpMul,
        OpMulEq,

        OpDiv,
        OpDivEq,

        OpMod,
        OpModEq,

        OpAssign,
        OpEq,
        OpNot,
        OpNeq,

        OpArrow,

        OpGt,
        OpLt,
        OpGtEq,
        OpLtEq,

        OpBitOr,
        OpBitAnd,
        OpBitOrEq,
        OpBitAndEq,
        OpLogOr,
        OpLogAnd,

        OpShiftLeft,
        OpShiftRight,

        OpShiftLeftEq,
        OpShiftRightEq,

        OpXor,
        OpXorEq,

        KwHash,

        KwAlignas,
        KwAlignof,
        KwAuto,
        KwBool,
        KwBreak,
        KwCase,
        KwChar,
        KwComplex,
        KwConst,
        KwContinue,
        KwDefault,
        KwDo,
        KwDouble,
        KwElse,
        KwEnum,
        KwExtern,
        KwFloat,
        KwFor,
        KwGeneric,
        KwGoto,
        KwIf,
        KwImaginary,
        KwInline,
        KwInt,
        KwLong,
        KwNoreturn,
        KwRegister,
        KwRestrict,
        KwReturn,
        KwHashhash,
        KwShort,
        KwSigned,
        KwSizeof,
        KwStatic,
        KwStatic_assert,
        KwStruct,
        KwSwitch,
        KwEllipsis,
        KwTypedef,
        KwTypeof,
        KwUnion,
        KwUnsigned,
        KwVoid,
        KwVolatile,
        KwWhile,
    }

    impl Keyword {
        fn to_string(&self) -> String {
            use Keyword::*;
            let p = match self {
                LBrace => "{",
                RBrace => "}",
                LBracket => "[",
                RBracket => "]",
                LParen => "(",
                RParen => ")",
                Colon => ":",
                Semi => ";",
                OpAdd => "+",
                OpInc => "++",
                OpAddEq => "+=",
                OpSub => "-",
                OpDec => "--",
                OpSubEq => "-=",

                OpMul => "*",
                OpMulEq => "*=",

                OpDiv => "/",
                OpDivEq => "/=",

                OpMod => "%",
                OpModEq => "%=",

                OpAssign => "=",
                OpEq => "==",
                OpNot => "!",
                OpNeq => "!=",

                OpArrow => "->",

                OpGt => ">",
                OpLt => "<",
                OpGtEq => ">=",
                OpLtEq => "<=",

                OpBitOr => "|",
                OpBitAnd => "&",
                OpBitOrEq => "|=",
                OpBitAndEq => "&=",
                OpLogOr => "||",
                OpLogAnd => "&&",

                OpShiftLeft => "<<",
                OpShiftRight => ">>",

                OpShiftLeftEq => "<<=",
                OpShiftRightEq => ">>=",

                OpXor => "^",
                OpXorEq => "^=",

                KwHash => "#",
                KwAlignas => "_Alignas",
                KwAlignof => "_Alignof",
                KwAuto => "auto",
                KwBool => "_Bool",
                KwBreak => "break",
                KwCase => "case",
                KwChar => "char",
                KwComplex => "_Complex",
                KwConst => "const",
                KwContinue => "continue",
                KwDefault => "default",
                KwDo => "do",
                KwDouble => "double",
                KwElse => "else",
                KwEnum => "enum",
                KwExtern => "extern",
                KwFloat => "float",
                KwFor => "for",
                KwGeneric => "_Generic",
                KwGoto => "goto",
                KwIf => "if",
                KwImaginary => "_Imaginary",
                KwInline => "inline",
                KwInt => "int",
                KwLong => "long",
                KwNoreturn => "_Noreturn",
                KwRegister => "register",
                KwRestrict => "restrict",
                KwReturn => "return",
                KwHashhash => "##",
                KwShort => "short",
                KwSigned => "signed",
                KwSizeof => "sizeof",
                KwStatic => "static",
                KwStatic_assert => "_Static_assert",
                KwStruct => "struct",
                KwSwitch => "switch",
                KwEllipsis => "...",
                KwTypedef => "typedef",
                KwTypeof => "typeof",
                KwUnion => "union",
                KwUnsigned => "unsigned",
                KwVoid => "void",
                KwVolatile => "volatile",
                KwWhile => "while",
            };

            p.to_string()
        }

        fn from(keyword: &str) -> Self {
            use Keyword::*;
            match keyword {
                "(" => LParen,
                ")" => RParen,
                "{" => LBrace,
                "}" => RBrace,
                "[" => LBracket,
                "]" => RBracket,
                ";" => Semi,
                ":" => Colon,
                "+" => OpAdd,
                "++" => OpInc,
                "+=" => OpAddEq,
                "-" => OpSub,
                "--" => OpDec,
                "-=" => OpSubEq,

                "*" => OpMul,
                "*=" => OpMulEq,

                "/" => OpDiv,
                "/=" => OpDivEq,

                "%" => OpMod,
                "%=" => OpModEq,

                "=" => OpAssign,
                "==" => OpEq,
                "!" => OpNot,
                "!=" => OpNeq,

                "->" => OpArrow,

                ">" => OpGt,
                "<" => OpLt,
                ">=" => OpGtEq,
                "<=" => OpLtEq,

                "|" => OpBitOr,
                "&" => OpBitAnd,
                "|=" => OpBitOrEq,
                "&=" => OpBitAndEq,
                "||" => OpLogOr,
                "&&" => OpLogAnd,

                "<<" => OpShiftLeft,
                ">>" => OpShiftRight,

                "<<=" => OpShiftLeftEq,
                ">>=" => OpShiftRightEq,

                "^" => OpXor,
                "^=" => OpXorEq,

                "#" => KwHash,
                "_Alignas" => KwAlignas,
                "_Alignof" => KwAlignof,
                "auto" => KwAuto,
                "_Bool" => KwBool,
                "break" => KwBreak,
                "case" => KwCase,
                "char" => KwChar,
                "_Complex" => KwComplex,
                "const" => KwConst,
                "continue" => KwContinue,
                "default" => KwDefault,
                "do" => KwDo,
                "double" => KwDouble,
                "else" => KwElse,
                "enum" => KwEnum,
                "extern" => KwExtern,
                "float" => KwFloat,
                "for" => KwFor,
                "_Generic" => KwGeneric,
                "goto" => KwGoto,
                "if" => KwIf,
                "_Imaginary" => KwImaginary,
                "inline" => KwInline,
                "int" => KwInt,
                "long" => KwLong,
                "_Noreturn" => KwNoreturn,
                "register" => KwRegister,
                "restrict" => KwRestrict,
                "return" => KwReturn,
                "##" => KwHashhash,
                "short" => KwShort,
                "signed" => KwSigned,
                "sizeof" => KwSizeof,
                "static" => KwStatic,
                "_Static_assert" => KwStatic_assert,
                "struct" => KwStruct,
                "switch" => KwSwitch,
                "..." => KwEllipsis,
                "typedef" => KwTypedef,
                "typeof" => KwTypeof,
                "union" => KwUnion,
                "unsigned" => KwUnsigned,
                "void" => KwVoid,
                "volatile" => KwVolatile,
                "while" => KwWhile,

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

        pub fn next(&mut self, expect: char) -> bool {
            let ch = self.read_char();
            id ch == Some(expect) {
                return true;
            }
            if let Some(ch) = ch {
                self.unread(ch);
            }
            false
        }
        pub fn peek(&mut self) -> Option<char> {
            let ch = self.read_char()?;
            self.unread(ch);
            Some(ch)
        }

        pub fn read_rep(&mut self, expect: char, kw1: Keyword, kw2: Keyword) -> Token {
            if self.next(expect) {
                self.make_keyword_token(kw1)
            } else {
                self.make_keyword_token(kw2)
            }
        }
        pub fn read_rep2(
            &mut self,
            expect1: char,
            kw1: Keyword,
            expect2: char,
            kw2: Keyword,
            kw3: Keyword,
        ) -> Token {
            if self.next(expect1) {
                self.make_keyword_token(kw1)
            } else if self.next(expect2) {
                self.make_keyword_token(kw2)
            } else {
                self.make_keyword_token(kw3)
            }
        }

        pub fn read_token(&mut self) -> Option<Token> {
            let ch = self.read_char()?;
            let file = &mut self.files[self.index];
            let pos = file.current_pos.clone();

            // TODO Are \f and \v also handled here
            if ch.is_whitespace() && ch != '\n' {
                return Some(self.read_space_token(pos));
            }

            let token = match ch {
                '\n' => self.make_newline_token(),
                ':' => self.read_rep('>', Keyword::RBracket, Keyword::Colon),

                '#' => self.read_rep('#', Keyword::KwHashhash, Keyword::KwHash),
                '+' => self.read_rep2('+', Keyword::OpInc, '=', Keyword::OpAddEq, Keyword::OpAdd),
                '*' => self.read_rep('=', Keyword::OpMulEq, Keyword::OpMul),
                '=' => self.read_rep('=', Keyword::OpEq, Keyword::OpAssign),
                '!' => self.read_rep('=', Keyword::OpNeq, Keyword::OpNot),
                '&' => self.read_rep2(
                    '&',
                    Keyword::OpLogAnd,
                    '=',
                    Keyword::OpBitAndEq,
                    Keyword::OpBitAnd,
                ),
                '|' => self.read_rep2(
                    '|',
                    Keyword::OpLogOr,
                    '=',
                    Keyword::OpBitOrEq,
                    Keyword::OpBitOr,
                ),
                '^' => self.read_rep('=', Keyword::OpXorEq, Keyword::OpXor),
                // TODO Read String
                // TODO Read Char
                '/' => self.read_rep('=', Keyword::OpDivEq, Keyword::OpDiv),
                'a'..='t' | 'v'..='z' | 'A'..='K' | 'M'..='T' | 'V'..='Z' => {
                    self.read_ident(ch, pos)
                }
                // TODO Read L U u
                '{' | '}' | '[' | ']' | '(' | ')' | '?' | ',' | '~' | ';' => {
                    self.make_keyword_token(Keyword::from(&ch.to_string()))
                }
                // TODO Read .
                '-' => {
                    if self.next('-') { self.make_keyword_token(Keyword::OpDec) }
                    else if self.next('>') { self.make_keyword_token(Keyword::Arrow) }
                    else if self.next('=') { self.make_keyword_token(Keyword::OpSubEq) }
                    else  { self.make_keyword_token(Keyword::OpSub) }
                }
                '<' => {
                    if self.next('<') { self.read_rep('=', Keyword::OpShiftLeftEq, Keyword::OpShiftLeft ) }
                    else if self.next('=') { self.make_keyword_token(Keyword::OpLtEq) }
                    else if self.next(':') { self.make_keyword_token(Keyword::LBracket) }
                    else if self.next('%') { self.make_keyword_token(Keyword::LBrace) }
                    else  { self.make_keyword_token(Keyword::OpLt) }

                }


                '0'..='9' => self.read_number(ch, pos),
                _ => panic!("Unrecognized character token {}", ch),
            };

            Some(token)
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
                last = ch;
            }

            let file = &mut self.files[self.index];
            let token_index = file.tokens;
            file.tokens += 1;
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

            let file = &mut self.files[self.index];
            let token_index = file.tokens;
            file.tokens += 1;
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

            let token_index = file.tokens;
            file.tokens += 1;
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

            let token_index = file.tokens;
            file.tokens += 1;

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

            let token_index = file.tokens;
            file.tokens += 1;
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
