use std::collections::HashMap;

use sexp::Sexp;
use sexp::SymbolHandle;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum ReadSexpErrorKind {
    UnclosedString,
    UnexpectedEOF,
    UnexpectedChar
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
            self.col = 0; // TODO find the right column number
        } else {
            self.col -= 1;
        }
    }
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

fn read_str(ps: &mut ParserState) -> Result<Sexp, ReadSexpError> {
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

fn quoted_sexp(s: Sexp) -> Sexp {
    let mut lst : Vec<Sexp> = Vec::new();
    lst.push(Sexp::Symbol(String::from("quote")));
    lst.push(s);
    Sexp::List(lst)
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

    assert_eq!(read_str(&mut ps2), Err(ReadSexpError{ line: 0, col: 8, kind: ReadSexpErrorKind::UnclosedString}));
}

#[test]
fn test_quoted_sexp() {
    assert_eq!(quoted_sexp(Sexp::Symbol(String::from("alamo"))),
               Sexp::List(vec!(Sexp::Symbol(String::from("quote")), Sexp::Symbol(String::from("alamo")))));
}

#[test]
fn test_skip_whitespace() {
    let mut ps1 = ParserState::new(String::from("   \nbamboleio"));

    ps1.skip_whitespace();
    assert_eq!(ps1.peek(), Some('b'));

    let mut ps2 = ParserState::new(String::from(" \n \n \n \n "));

    ps2.skip_whitespace();
    assert_eq!(ps2.peek(), None);
}