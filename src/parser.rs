use std::collections::HashMap;

use sexp::Sexp;
use sexp::SymbolHandle;

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

impl ParserState {
    pub fn new(s: String) -> Self {
        ParserState { chars: s.chars().collect(), ix: 0, line: 0, col: 0 }
    }

    pub fn peek(&self) -> Option<char> {
        if self.ix < self.chars.len()  {
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
}

// the type for read results
type ReadResult = Result<Sexp, ReadSexpError>;

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
    assert!(ps.next() == Some('"'), "Unexpected start of string");
    let mut res = String::new();
    let mut mc = ps.next();

    while mc != None && mc != Some('"') {
        res.push(mc.unwrap());  // mc != None
        mc = ps.next();
    }

    if mc == None {
        Err(ReadSexpError { line: ps.line, col: ps.col, kind: ReadSexpErrorKind::UnclosedString })
    } else {
        Ok(Sexp::Str(res))
    }
}

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

// TODO include the "peculiar id"
fn id_char(c: char) -> bool {
    c.is_digit(10) || id_first_char(c) || c == '+' || c == '-' || c == '.' || c == '@'
}

fn read_identifier(ps: &mut ParserState, symtbl: &mut SymbolTable) -> ReadResult {
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
            Ok(Sexp::Symbol(symtbl.intern(idstr)))
        }
    }
}

// TODO support for #\space and #\newline
fn read_char(ps: &mut ParserState) -> ReadResult {
    match ps.next() {
        None => Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedEOF)),
        Some(c) => Ok(Sexp::Char(c))
    }
}

fn read_vector(ps: &mut ParserState, symtbl: &mut SymbolTable) -> ReadResult {
    let v : Vec<Sexp> = Vec::new();
    Ok(Sexp::Vector(v))  // TODO implement
}

fn read_prefix_number_or_error(ps: &mut ParserState, c: char) -> ReadResult {
    Ok(Sexp::Number(0))  // TODO implement
}

fn read_hash(ps: &mut ParserState, symtbl: &mut SymbolTable) -> ReadResult {
    match ps.next() {
        None => Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedEOF)),
        Some(c) => {
            if c != '#' {
                return Err(sexp_error_current_location(ps, ReadSexpErrorKind::ExpectedHash));
            }

            match ps.next() {
                None => Err(sexp_error_current_location(ps, ReadSexpErrorKind::UnexpectedEOF)),
                Some('t') => Ok(Sexp::Bool(true)),
                Some('f') => Ok(Sexp::Bool(false)),
                Some('\\') => read_char(ps),
                Some('(') => read_vector(ps, symtbl),
                Some(ch) => read_prefix_number_or_error(ps, ch)
            }
        }
    }
}

//fn read_atom(ps: &mut ParserState) -> Sexp {
//    match ps.peek() {
//        '#' => read_hash(ps),
//        '"' => read_str(ps),
//        '\'' => quoted_sexp()
//    }
//}
//
//fn read_sexp(ps: &mut ParserState) -> Sexp {
//    match ps.peek() {
//        '(' => read_list(ps),
//        _ => read_atom(ps)
//    }
//}
//
//pub fn parse_atom(ps: &mut ParserState) -> Sexp {
//    match ps.peek() {
//        '#' => parse_hash(ps),
//        '"' => parse_string(ps),
//        '\'' => quoted_sexp()
//    }
//}
//
//// parses a top-level sexp (a list)
//// assumes the opening parenthesis was already consumed
//pub fn parse_sexp(ps: &mut ParserState) -> Sexp {
//    let mut res : Vec<Sexp> = Vec::new();
//
//    while ps.peek() != ')' {
//        match ps.peek() {
//            '(' => { ps.next(); res.push(parse_sexp(ps)) },
//            _ => res.push(parse_atom(ps))
//        }
//    }
//
//    Sexp::List(res)
//}

#[test]
fn test_read_str() {
    let mut ps1 = ParserState::new(String::from("\"Alamo hapsbar hammyn\""));

    assert_eq!(read_str(&mut ps1), Ok(Sexp::Str(String::from("Alamo hapsbar hammyn"))));

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
fn symbol_from_str(s: &str, st: &mut SymbolTable) -> Sexp {
    Sexp::Symbol(st.intern(String::from(s)))
}

#[test]
fn test_read_id() {
    let mut ps1 = ParserState::new(String::from("batraqueo"));
    let mut symtbl = SymbolTable::new();

    assert_eq!(read_identifier(&mut ps1, &mut symtbl),
               Ok(symbol_from_str("batraqueo", &mut symtbl)));
}

#[test]
fn test_read_hash() {
    let mut ps1 = ParserState::new(String::from("#t #f #\\A"));
    let mut symtbl = SymbolTable::new();

    assert_eq!(read_hash(&mut ps1, &mut symtbl), Ok(Sexp::Bool(true)));
    ps1.skip_whitespace();
    assert_eq!(read_hash(&mut ps1, &mut symtbl), Ok(Sexp::Bool(false)));
    ps1.skip_whitespace();
    assert_eq!(read_hash(&mut ps1, &mut symtbl), Ok(Sexp::Char('A')));
}
