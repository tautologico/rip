use std::collections::HashMap;

use sexp::Sexp;
use sexp::SymbolHandle;

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

enum TokenValue {
    LParen,
    RParen,
    HashParen,
    Comma,
    Quote,
    Backquote,
    Number,
    Str(String),
    Id(SymbolHandle)
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Token {
    start: Loc,
    end: Loc,
    contents: String
}

impl Token {
    pub fn new(start: Loc, s: String) -> Token {
        let eloc = Loc::new(start.line, start.col + s.len(), start.pos + s.len());
        Token { start: start, end: eloc, contents: s }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum SexpValue {
    Number(String),
    Str(String),
    Symbol(SymbolHandle),
    Bool(bool),
    Char(char),
    Vector(Vec<SynSexp>),
    List(Vec<SynSexp>)
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct SynSexp {
    start: Loc,
    end: Loc,
    val: SexpValue
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum ReadSexpErrorKind {
    UnclosedString,
    UnexpectedEOF,
    UnexpectedChar(char),
    ExpectedHash
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct ReadSexpError {
    line: usize,
    col: usize,
    kind: ReadSexpErrorKind
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum LexErrorKind {
    UnclosedString,
    UnexpectedEOF,
    UnexpectedChar(char),
    ExpectedHash
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct LexError {
    line: usize,
    col: usize,
    kind: LexErrorKind
}

type LexResult = Result<Token, LexError>;

pub struct ParserState {
    chars: Vec<char>,
    ix: usize,
    line: usize,
    col: usize
}

fn is_delimiter(c: char) -> bool {
    c.is_whitespace() || c == ')' || c == ';'
}

impl ParserState {
    pub fn new(s: String) -> Self {
        ParserState { chars: s.chars().collect(), ix: 0, line: 0, col: 0 }
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
        if self.chars[self.ix-1] == '\n' {   // TODO what about tabs?
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

    fn read_string_literal(&mut self) -> Token {
        let sloc = self.current_loc();
        let open = self.next();

        assert!( open == Some('"'), "read_string_literal called, expected double quotes" );

        let mut res = String::new();
        res.push(open.unwrap());
        let mut mc = self.next();

        while mc != None && mc != Some('"') && mc != Some('\n') {
            res.push(mc.unwrap());  // mc != None
            mc = self.next();
        }

        if mc == Some('"') {
            res.push(mc.unwrap());
        }
        Token { start: sloc, end: self.current_loc(), contents: res }
    }

    fn read_upto_delimiter(&mut self, res: &mut String) {
        let mut mc = self.next();

        while mc != None && !is_delimiter(mc.unwrap()) {
            res.push(mc.unwrap());
            mc = self.next();
        }

        if mc != None {
            self.backtrack();
        }
    }

    fn read_hash_token(&mut self) -> Token {
        let sloc = self.current_loc();
        let mut res = String::new();
        let mut mc = self.next();

        assert!( mc == Some('#'), "read_hash_token: expected #" );

        res.push(mc.unwrap());

        match self.next() {
            None => (),
            Some('(') => res.push('('),
            Some(c) => {
                res.push(c);
                self.read_upto_delimiter(&mut res);
            }
        }
        Token { start: sloc, end: self.current_loc(), contents: res }
    }

    fn read_token(&mut self) -> Token {
        let sloc = self.current_loc();
        let mut res = String::new();
        self.read_upto_delimiter(&mut res);
        Token { start: sloc, end: self.current_loc(), contents: res }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        match self.peek() {
            None => None,
            Some(';') => { self.skip_comment(); self.next_token() },
            Some(c) => {
                match c {
                    '(' | ')' | '\'' | '`' | ',' => {
                        let sloc = self.current_loc();
                        let mut res = String::new();
                        self.next();
                        let eloc = self.current_loc();
                        res.push(c);
                        Some(Token { start: sloc, end: eloc, contents: res })
                    },
                    '"' => Some(self.read_string_literal()),
                    '#' => Some(self.read_hash_token()),
                    _ => Some(self.read_token())
                }

            }
        }
    }

    fn current_loc(&self) -> Loc {
        Loc { line: self.line, col: self.col, pos: self.ix }
    }

    fn make_sexp(&self, val: SexpValue, start: Loc) -> SynSexp {
        let endloc = Loc { line: self.line, col: self.col, pos: self.ix };
        SynSexp { start: start, end: endloc, val: val }
    }
}

// the type for read results
type ReadResult = Result<SynSexp, ReadSexpError>;

fn sexp_error_current_location(ps: &ParserState, kind: ReadSexpErrorKind) -> ReadSexpError {
    ReadSexpError { line: ps.line, col: ps.col, kind: kind }
}

#[test]
fn test_peek() {
    let ps1 = ParserState::new(String::from("Armoir"));

    assert_eq!(ps1.peek(), Some('A'));

    let ps2 = ParserState::new(String::from(""));

    assert_eq!(ps2.peek(), None);
}

#[test]
fn test_next() {
    let mut ps1 = ParserState::new(String::from("Armoir"));

    assert_eq!(ps1.next(), Some('A'));
    assert_eq!(ps1.next(), Some('r'));
    assert_eq!(ps1.next(), Some('m'));
    assert_eq!(ps1.peek(), Some('o'));
    assert_eq!(ps1.next(), Some('o'));
    assert_eq!(ps1.next(), Some('i'));
    assert_eq!(ps1.next(), Some('r'));
    assert_eq!(ps1.next(), None);
}

pub struct SymbolTable {
    symbols: HashMap<String, SymbolHandle>,
    next: SymbolHandle
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable { symbols: HashMap::new(), next: 0 }
    }

    pub fn intern(&mut self, s: String) -> SymbolHandle {
        match self.symbols.get(&s) {
            Some(&h) => h,
            None => {
                let res = self.next;
                self.symbols.insert(s, res);
                self.next += 1;
                res
            }
        }
    }

    pub fn intern_str(&mut self, s: &str) -> SymbolHandle {
        self.intern(String::from(s))
    }
}

#[test]
fn test_symbol_table() {
    let mut symtbl = SymbolTable::new();

    assert_eq!(symtbl.intern_str("quote"), 0);
    assert_eq!(symtbl.intern_str("cond"), 1);
    assert_eq!(symtbl.intern_str("loop"), 2);
    assert_eq!(symtbl.intern(String::from("quote")), 0);
    assert_eq!(symtbl.intern(String::from("loop")), 2);
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

#[test]
fn test_skip_whitespace() {
    let mut ps1 = ParserState::new(String::from("   \nbamboleio"));

    ps1.skip_whitespace();
    assert_eq!(ps1.peek(), Some('b'));
    assert_eq!(ps1.line, 1);
    assert_eq!(ps1.col, 0);

    let mut ps2 = ParserState::new(String::from(" \n \n \n \n "));

    ps2.skip_whitespace();
    assert_eq!(ps2.peek(), None);
    assert_eq!(ps2.line, 4);
    assert_eq!(ps2.col, 1);
}

#[test]
fn test_backtrack() {
    let mut ps1 = ParserState::new(String::from("   bamboleo\nbambolea"));

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

#[cfg(test)]
fn symbol_from_str(s: &str, st: &mut SymbolTable) -> SexpValue {
    SexpValue::Symbol(st.intern(String::from(s)))
}

#[test]
fn test_tokenizer_1() {
    let mut ps = ParserState::new(String::from("(define abs '(123 #\\a #f))"));

    assert_eq!(ps.next_token(), Some(Token::new( Loc::zero(), String::from("(") )));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 1, 1), String::from("define") )));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 8, 8), String::from("abs") )));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 12, 12), String::from("'") )));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 13, 13), String::from("(") )));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 14, 14), String::from("123") )));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 18, 18), String::from("#\\a") )));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 22, 22), String::from("#f") )));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 24, 24), String::from(")") )));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 25, 25), String::from(")") )));
    assert_eq!(ps.next_token(), None);
}

#[test]
fn test_tokenizer_2() {
    let mut ps = ParserState::new(String::from("#(77.5 #f `(abs ,cond))"));

    assert_eq!(ps.next_token(), Some(Token::new( Loc::zero(), String::from("#(") )));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 2, 2), String::from("77.5"))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 7, 7), String::from("#f"))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 10, 10), String::from("`"))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 11, 11), String::from("("))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 12, 12), String::from("abs"))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 16, 16), String::from(","))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 17, 17), String::from("cond"))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 21, 21), String::from(")"))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 22, 22), String::from(")"))));
    assert_eq!(ps.next_token(), None);
}

#[test]
fn test_tokenizer_3() {
    let mut ps = ParserState::new(String::from("'(abet #\\space \"shumaguma ' matalka #\")"));

    assert_eq!(ps.next_token(), Some(Token::new( Loc::zero(), String::from("'") )));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 1, 1), String::from("("))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 2, 2), String::from("abet"))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 7, 7), String::from("#\\space"))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 15, 15), String::from("\"shumaguma ' matalka #\""))));
    assert_eq!(ps.next_token(), Some(Token::new( Loc::new(0, 38, 38), String::from(")"))));
    assert_eq!(ps.next_token(), None);
}
