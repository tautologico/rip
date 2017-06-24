use num::Num;
use symtbl::SymbolHandle;
use symtbl::SymbolTable;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Loc {
    line: usize,
    col: usize,
    pos: usize
}

impl Loc {
    pub fn zero() -> Loc {
        Loc { line: 0, col: 0, pos: 0 }
    }

    pub fn new(l: usize, c: usize, p: usize) -> Loc {
        Loc { line: l, col: c, pos: p }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum TokenValue {
    Eof,
    LParen,
    RParen,
    HashParen,
    Comma,
    Quote,
    Backquote,
    Bool(bool),
    Number(Num),
    Str(String),
    Symbol(SymbolHandle)
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Token {
    start: Loc,
    end: Loc,
    value: TokenValue
}

impl Token {
    pub fn new(start: Loc, end: Loc, v: TokenValue) -> Token {
        Token { start: start, end: end, value: v }
    }

    pub fn string(start: Loc, end: Loc, s: String) -> Token {
        Token { start: start, end: end, value: TokenValue::Str(s) }
    }

    pub fn boolean(start: Loc, end: Loc, b: bool) -> Token {
        Token { start: start, end: end, value: TokenValue::Bool(b) }
    }

    pub fn symbol(start: Loc, end: Loc, handle: SymbolHandle) -> Token {
        Token { start: start, end: end, value: TokenValue::Symbol(handle) }
    }

    pub fn number(start: Loc, end: Loc, i: i64) -> Token {
        Token { start: start, end: end, value: TokenValue::Number(Num::Exact(i)) }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum LexErrorKind {
    UnclosedString,
    UnexpectedEOF,
    UnexpectedChar(char),
    NotImplemented,
    ExpectedHash
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct LexError {
    line: usize,
    col: usize,
    kind: LexErrorKind
}

type LexResult = Result<Token, LexError>;

pub struct Buffer {
    chars: Vec<char>,
    ix: usize,
    line: usize,
    col: usize
}

impl Buffer {
    pub fn new(s: String) -> Self {
        Buffer { chars: s.chars().collect(), ix: 0, line: 0, col: 0 }
    }

    pub fn peek(&self) -> Option<char> {
        if self.ix < self.chars.len() {
            Some(self.chars[self.ix])
        } else {
            None
        }
    }

    fn advance_ix(&mut self) {
        self.ix += 1;

        // adjust column and line numbers
        if self.chars[self.ix - 1] == '\n' {
            // TODO what about tabs?
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }

    pub fn next(&mut self) -> Option<char> {
        if self.ix < self.chars.len() {
            let res = self.chars[self.ix];
            self.advance_ix();
            Some(res)
        } else {
            None
        }
    }

    pub fn skip_comment(&mut self) {
        while self.ix < self.chars.len() && self.chars[self.ix] != '\n' {
            self.advance_ix();
        }
    }

    pub fn skip_whitespace(&mut self) {
        while self.ix < self.chars.len() && self.chars[self.ix].is_whitespace() {
            self.advance_ix();
        }
    }

    pub fn backtrack(&mut self) {
        self.ix -= 1;
        if self.chars[self.ix] == '\n' {
            self.line -= 1;

            // go back to the previous line to find out the column number
            let mut cs = self.ix - 1;
            while cs > 0 && self.chars[cs] != '\n' {
                cs -= 1;
            }
            self.col = self.ix - cs;
            if self.chars[cs] == '\n' {
                self.col -= 1;
            }
        } else {
            self.col -= 1;
        }
    }

    fn current_loc(&self) -> Loc {
        Loc { line: self.line, col: self.col, pos: self.ix }
    }

    fn lexer_error_current_loc(&self, kind: LexErrorKind) -> LexError {
        LexError { line: self.line, col: self.col, kind: kind }
    }
}

#[cfg(test)]
mod buffer_tests {
    use super::*;

    #[test]
    fn peek() {
        let ps1 = Buffer::new(String::from("Armoir"));

        assert_eq!(ps1.peek(), Some('A'));

        let ps2 = Buffer::new(String::from(""));

        assert_eq!(ps2.peek(), None);
    }

    #[test]
    fn next() {
        let mut ps1 = Buffer::new(String::from("Armoir"));

        assert_eq!(ps1.next(), Some('A'));
        assert_eq!(ps1.next(), Some('r'));
        assert_eq!(ps1.next(), Some('m'));
        assert_eq!(ps1.peek(), Some('o'));
        assert_eq!(ps1.next(), Some('o'));
        assert_eq!(ps1.next(), Some('i'));
        assert_eq!(ps1.next(), Some('r'));
        assert_eq!(ps1.next(), None);
    }

    #[test]
    fn skip_whitespace() {
        let mut ps1 = Buffer::new(String::from("   \nbamboleio"));

        ps1.skip_whitespace();
        assert_eq!(ps1.peek(), Some('b'));
        assert_eq!(ps1.line, 1);
        assert_eq!(ps1.col, 0);

        let mut ps2 = Buffer::new(String::from(" \n \n \n \n "));

        ps2.skip_whitespace();
        assert_eq!(ps2.peek(), None);
        assert_eq!(ps2.line, 4);
        assert_eq!(ps2.col, 1);
    }

    #[test]
    fn backtrack() {
        let mut ps1 = Buffer::new(String::from("   bamboleo\nbambolea"));

        ps1.skip_whitespace();
        assert_eq!(ps1.next(), Some('b'));
        assert_eq!(ps1.next(), Some('a'));
        assert_eq!(ps1.next(), Some('m'));
        assert_eq!(ps1.next(), Some('b'));
        assert_eq!(ps1.next(), Some('o'));
        assert_eq!(ps1.next(), Some('l'));
        assert_eq!(ps1.next(), Some('e'));
        assert_eq!(ps1.next(), Some('o'));

        assert_eq!(ps1.line, 0);
        assert_eq!(ps1.col, 11);

        ps1.skip_whitespace();
        assert_eq!(ps1.peek(), Some('b'));
        assert_eq!(ps1.line, 1);
        assert_eq!(ps1.col, 0);

        ps1.backtrack();
        assert_eq!(ps1.peek(), Some('\n'));
        assert_eq!(ps1.line, 0);
        assert_eq!(ps1.col, 11);
    }
}

pub struct Lexer {
    buffer: Buffer
}

fn is_delimiter(c: char) -> bool {
    c.is_whitespace() || c == ')' || c == ';'
}

fn id_symbol_char(c: char) -> bool {
    match c {
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '='
        | '>' | '?' | '^' | '_' | '~' => true,
        _ => false
    }
}

fn id_first_char(c: char) -> bool {
    c.is_alphabetic() || id_symbol_char(c)
}

fn id_char(c: char) -> bool {
    c.is_digit(10) || id_first_char(c) || c == '+' || c == '-' || c == '.' || c == '@'
}

impl Lexer {
    fn new(s: String) -> Lexer {
        Lexer { buffer: Buffer::new(s) }
    }

    fn read_string_literal(&mut self) -> LexResult {
        let sloc = self.buffer.current_loc();

        assert!( self.buffer.next() == Some('"'), "read_string_literal called, expected double quotes" );

        let mut res = String::new();
        let mut mc = self.buffer.next();

        while mc != None && mc != Some('"') && mc != Some('\n') {
            res.push(mc.unwrap());  // mc != None
            mc = self.buffer.next();
        }

        if mc != Some('"') {
            Err(self.buffer.lexer_error_current_loc(LexErrorKind::UnclosedString))
        } else {
            Ok(Token::string(sloc, self.buffer.current_loc(), res))
        }
    }

    fn read_hash_number(&mut self) -> LexResult {
        Err(self.buffer.lexer_error_current_loc(LexErrorKind::NotImplemented))
    }

    fn read_hash_token(&mut self) -> LexResult {
        let sloc = self.buffer.current_loc();
        let mut mc = self.buffer.next();

        assert!( mc == Some('#'), "read_hash_token: expected #" );

        match self.buffer.next() {
            None => Err(self.buffer.lexer_error_current_loc(LexErrorKind::UnexpectedEOF)),
            Some('(') => Ok(Token{ start: sloc, end: self.buffer.current_loc(), value: TokenValue::HashParen }),
            Some('t') => Ok(Token::boolean(sloc, self.buffer.current_loc(), true)),
            Some('f') => Ok(Token::boolean(sloc, self.buffer.current_loc(), false)),
            Some(c) => self.read_hash_number()
        }
    }

    fn read_id(&mut self, c: char, sloc: Loc, symtbl: &mut SymbolTable) -> LexResult {
        let mut idstr = String::new();

        idstr.push(c);

        let mut mc = self.buffer.next();
        while mc != None && id_char(mc.unwrap()) && !is_delimiter(mc.unwrap()) {
            idstr.push(mc.unwrap());
            mc = self.buffer.next();
        }

        if !id_char(mc.unwrap()) {
            Err(self.buffer.lexer_error_current_loc(LexErrorKind::UnexpectedChar(c)))
        } else {
            Ok(Token::symbol(sloc, self.buffer.current_loc(), symtbl.intern(idstr)))
        }
    }

    fn read_upto_delimiter(&mut self, res: &mut String) {
        let mut mc = self.buffer.next();

        while mc != None && !is_delimiter(mc.unwrap()) {
            res.push(mc.unwrap());
            mc = self.buffer.next();
        }

        if mc != None {
            self.buffer.backtrack();
        }
    }

    // TODO implement real parsing for scheme numbers
    fn read_number(&mut self, c: char, sloc: Loc) -> LexResult {
        let mut res = String::new();
        res.push(c);
        self.read_upto_delimiter(&mut res);
        match res.parse::<i64>() {
            Ok(i) => Ok(Token::number(sloc, self.buffer.current_loc(), i)),
            _ =>     Err(self.buffer.lexer_error_current_loc(LexErrorKind::UnexpectedEOF))
        }
    }

    fn number_or_id_token(&mut self, symtbl: &mut SymbolTable) -> LexResult {
        let sloc = self.buffer.current_loc();
        let mut res = String::new();
        //self.read_upto_delimiter(&mut res);

        match self.buffer.next() {
            None => panic!("number_or_id_token called at EOF"),
            Some(c) => {
                if id_first_char(c) {
                    self.read_id(c, sloc, symtbl)
                } else if c.is_numeric() {
                    self.read_number(c, sloc)    // TODO R5RS allows starting number literal with a dot
                } else {
                    Err(self.buffer.lexer_error_current_loc(LexErrorKind::UnexpectedChar(c)))
                }
            }
        }
    }

    fn eof_token(&mut self) -> LexResult {
        let sloc = self.buffer.current_loc();
        Ok(Token { start: sloc.clone(), end: sloc, value: TokenValue::Eof })
    }

    fn next_token(&mut self, symtbl: &mut SymbolTable) -> LexResult {
        self.buffer.skip_whitespace();
        match self.buffer.peek() {
            None => self.eof_token(),
            Some(';') => { self.buffer.skip_comment(); self.next_token(symtbl) },
            Some(c) => {
                match c {
                    '(' | ')' | '\'' | '`' | ',' => {
                        let sloc = self.buffer.current_loc();
                        self.buffer.next();
                        let eloc = self.buffer.current_loc();
                        let v = match c {
                            '(' => TokenValue::LParen,
                            ')' => TokenValue::RParen,
                            '\'' => TokenValue::Quote,
                            '`' => TokenValue::Backquote,
                            ',' => TokenValue::Comma,
                            _ => panic!("next_token: char changed")
                        };
                        Ok(Token { start: sloc, end: eloc, value: v })
                    },
                    '"' => self.read_string_literal(),
                    '#' => self.read_hash_token(),
                    _ => self.number_or_id_token(symtbl)
                }

            }
        }
    }
}


#[cfg(test)]
mod lexer_test {
    use super::*;

    #[test]
    fn lexer1() {
        let mut lexer = Lexer::new(String::from("  456 shaman   #t"));
        let mut symtbl = SymbolTable::new();

        assert_eq!(lexer.next_token(&mut symtbl),
                   Ok(Token::number(Loc { line: 0, col: 2, pos: 2 },
                                    Loc { line: 0, col: 5, pos: 5 },
                                    456)));

        assert_eq!(lexer.next_token(&mut symtbl),
                   Ok(Token::symbol(Loc { line: 0, col: 6, pos: 6 },
                                    Loc { line: 0, col: 12, pos: 12 },
                                    symtbl.intern(String::from("shaman")))));

        assert_eq!(lexer.next_token(&mut symtbl),
                   Ok(Token::boolean(Loc { line: 0, col: 15, pos: 15 },
                                     Loc { line: 0, col: 17, pos: 17 },
                                     true)));
    }
}