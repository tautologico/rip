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

fn read_str(ps: &mut ParserState) -> ReadResult {
    let sloc = ps.current_loc();
    assert!(ps.next() == Some('"'), "Unexpected start of string");
    let mut res = String::new();
    let mut mc = ps.next();

    while mc != None && mc != Some('"') && mc != Some('\n') {
        res.push(mc.unwrap());  // mc != None
        mc = ps.next();
    }

    if mc == None || mc == Some('\n') {
        Err(ReadSexpError { line: ps.line, col: ps.col, kind: ReadSexpErrorKind::UnclosedString })
    } else {
        Ok(ps.make_sexp(SexpValue::Str(res), sloc))
    }
}

// TODO change to SynSexp
fn quoted_sexp(s: Sexp, symtbl: &mut SymbolTable) -> Sexp {
    let mut lst : Vec<Sexp> = Vec::new();
    lst.push(Sexp::Symbol(symtbl.intern(String::from("quote"))));
    lst.push(s);
    Sexp::List(lst)
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

fn read_identifier(ps: &mut ParserState, symtbl: &mut SymbolTable) -> ReadResult {
    let sloc = ps.current_loc();
    let mut idstr = String::new();
    match ps.next() {
        None => Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedEOF)),
        Some(c) => {
            if !id_first_char(c) {
                return Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedChar(c)));
            }

            let mut ch = c;
            while id_char(ch) {
                idstr.push(ch);
                match ps.next() {
                    None => break,
                    Some(c2) => ch = c2
                }
            }
            Ok(ps.make_sexp(SexpValue::Symbol(symtbl.intern(idstr)), sloc))
        }
    }
}

// TODO support for #\space and #\newline
fn read_char(ps: &mut ParserState, sloc: Loc) -> ReadResult {
    match ps.next() {
        None => Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedEOF)),
        Some(c) => Ok(ps.make_sexp(SexpValue::Char(c), sloc))
    }
}

fn read_vector(ps: &mut ParserState, symtbl: &mut SymbolTable, sloc: Loc) -> ReadResult {
    let v : Vec<SynSexp> = Vec::new();
    Ok(ps.make_sexp(SexpValue::Vector(v), sloc))  // TODO implement
}

fn read_prefix_number_or_error(ps: &mut ParserState, c: char, sloc: Loc) -> ReadResult {
    Ok(ps.make_sexp(SexpValue::Number(String::from("0")), sloc))  // TODO implement
}

fn read_hash(ps: &mut ParserState, symtbl: &mut SymbolTable) -> ReadResult {
    let sloc = ps.current_loc();
    match ps.next() {
        None => Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedEOF)),
        Some(c) => {
            if c != '#' {
                return Err(sexp_error_current_location(ps, ReadSexpErrorKind::ExpectedHash));
            }

            match ps.next() {
                None => Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedEOF)),
                Some('t') => Ok(ps.make_sexp(SexpValue::Bool(true), sloc)),
                Some('f') => Ok(ps.make_sexp(SexpValue::Bool(false), sloc)),
                Some('\\') => read_char(ps, sloc),
                Some('(') => read_vector(ps, symtbl, sloc),
                Some(ch) => read_prefix_number_or_error(ps, ch, sloc)
            }
        }
    }
}

fn read_number(ps: &mut ParserState) -> ReadResult {
    let sloc = ps.current_loc();
    let mut nstr = String::new();
    match ps.next() {
        None => Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedEOF)),
        Some(c) => {
            let mut ch = c;
            while ch.is_numeric() || ch == '.' {
                nstr.push(ch);
                match ps.next() {
                    None => break,
                    Some(c2) => ch = c2
                }
            }
            if ch.is_numeric() || ch == '.' {
                Ok(ps.make_sexp(SexpValue::Number(nstr), sloc))
            } else {
                Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedChar(ch)))
            }
        }
    }
}

fn read_quoted(ps: &mut ParserState, symtbl: &mut SymbolTable) -> ReadResult {
    let sloc = ps.current_loc();
    read_sexp(ps, symtbl).and_then(|s| {
        let mut v : Vec<SynSexp> = Vec::new();
        v.push(SynSexp { start: sloc.clone(), end: sloc.clone(),
                         val: SexpValue::Symbol(symtbl.intern(String::from("quote"))) });
        let eloc = s.end.clone();
        v.push(s);
        Ok(SynSexp { start: sloc, end: eloc, val: SexpValue::List(v) })
    })
}

pub fn read_sexp(ps: &mut ParserState, symtbl: &mut SymbolTable) -> ReadResult {
    match ps.peek() {
        //Some('(') => read_list(ps, symtbl),
        Some('#') => read_hash(ps, symtbl),
        Some('\'') => read_quoted(ps, symtbl),
        Some(c) => {
            if id_first_char(c) {
                read_identifier(ps, symtbl)
            } else if c.is_digit(10) {
                read_number(ps)
            } else {
                Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedChar(c)))
            }
        },
        None => Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedEOF))
    }
}

#[test]
fn test_read_str() {
    let mut ps1 = ParserState::new(String::from("\"Alamo hapsbar hammyn\""));

    assert_eq!(read_str(&mut ps1),
               Ok(SynSexp { start: Loc::zero(),
                            end: Loc { line: 0, col: 22, pos: 22 },
                            val: SexpValue::Str(String::from("Alamo hapsbar hammyn")) } ));

    let mut ps2 = ParserState::new(String::from("\"tapioca"));

    assert_eq!(read_str(&mut ps2),
               Err(ReadSexpError{ line: 0, col: 8, kind: ReadSexpErrorKind::UnclosedString}));
}

#[test]
fn test_quoted_sexp() {
    let mut symtbl = SymbolTable::new();
    let quote_sym = symtbl.intern(String::from("quote"));
    let alamo_sym = symtbl.intern(String::from("alamo"));
    assert_eq!(quoted_sexp(Sexp::Symbol(alamo_sym), &mut symtbl),
               Sexp::List(vec!(Sexp::Symbol(quote_sym), Sexp::Symbol(alamo_sym))));
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
fn test_read_id() {
    let mut ps1 = ParserState::new(String::from("batraqueo"));
    let mut symtbl = SymbolTable::new();

    assert_eq!(read_identifier(&mut ps1, &mut symtbl),
               Ok(SynSexp { start: Loc::zero(),
                            end: Loc { line: 0, col: 9, pos: 9 },
                            val: symbol_from_str("batraqueo", &mut symtbl) } ));
}

#[test]
fn test_read_hash() {
    let mut ps1 = ParserState::new(String::from("#t #f #\\A"));
    let mut symtbl = SymbolTable::new();

    //assert_eq!(read_hash(&mut ps1, &mut symtbl), Ok(Sexp::Bool(true)));
    //ps1.skip_whitespace();
    //assert_eq!(read_hash(&mut ps1, &mut symtbl), Ok(Sexp::Bool(false)));
    //ps1.skip_whitespace();
    //assert_eq!(read_hash(&mut ps1, &mut symtbl), Ok(Sexp::Char('A')));
}

#[test]
fn test_read_sexp() {
    let mut ps1 = ParserState::new(String::from("1234"));
    let mut symtbl = SymbolTable::new();

    assert_eq!(read_sexp(&mut ps1, &mut symtbl),
               Ok(SynSexp { start: Loc::zero(), end: Loc { line: 0, col: 4, pos: 4 },
                            val: SexpValue::Number(String::from("1234")) }));
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