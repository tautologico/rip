use std::collections::HashMap;

use sexp::Sexp;
use sexp::SymbolHandle;

pub struct ParserState {
    chars: Vec<char>,
    ix: usize
}

impl ParserState {
    pub fn new(s: String) -> Self {
        ParserState { chars: s.chars().collect(), ix: 0 }
    }

    pub fn peek(&self) -> char {
        self.chars[self.ix]
    }

    pub fn next(&mut self) -> char {
        let res = self.chars[self.ix];
        self.ix += 1;
        res
    }
}

#[test]
fn test_peek() {
    let ps1 = ParserState::new(String::from("Aussonderungsaxiom"));

    assert_eq!(ps1.peek(), 'A');
}

#[test]
fn test_next() {
    let mut ps1 = ParserState::new(String::from("Aussonderungsaxiom"));

    assert_eq!(ps1.next(), 'A');
    assert_eq!(ps1.next(), 'u');
    assert_eq!(ps1.next(), 's');
    assert_eq!(ps1.peek(), 's');
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

fn read_str(ps: &mut ParserState) -> Sexp {
    assert!(ps.next() == '"', "Unexpected start of string");
    let mut res = String::new();
    let mut c = ps.next();

    while c != '"' {
        res.push(c);
        c = ps.next();
    }

    Sexp::Str(res)
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

    assert_eq!(read_str(&mut ps1), Sexp::Str(String::from("Alamo hapsbar hammyn")));
}

#[test]
fn test_quoted_sexp() {
    assert_eq!(quoted_sexp(Sexp::Symbol(String::from("alamo"))),
               Sexp::List(vec!(Sexp::Symbol(String::from("quote")), Sexp::Symbol(String::from("alamo")))));
}