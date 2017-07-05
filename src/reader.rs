use lexer::Lexer;
use lexer::LexError;
use lexer::LexErrorKind;
use lexer::Loc;
use lexer::Token;
use lexer::TokenValue;

use num::Num;

use symtbl::SymbolTable;
use symtbl::SymbolHandle;

use std::fmt;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum SexpValue {
    Number(Num),
    Str(String),
    Symbol(SymbolHandle),
    Bool(bool),
    Char(char),
    Vector(Vec<Sexp>),
    List(Vec<Sexp>)
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Sexp {
    start: Loc,
    end: Loc,
    value: SexpValue
}

pub struct SexpFormatter<'a> {
    s: &'a Sexp,
    symtbl: &'a SymbolTable
}

impl Sexp {
    fn new(start: Loc, end: Loc, val: SexpValue) -> Sexp {
        Sexp { start: start, end: end, value: val }
    }

    pub fn print(&self, symtbl: &SymbolTable) {
        match self.value {
            SexpValue::Number(ref n) => n.print(),
            SexpValue::Str(ref s) => print!("\"{}\"", s),
            SexpValue::Symbol(sh) => symtbl.print_symbol(sh),
            SexpValue::Bool(b) => if b { print!("#t"); } else { print!("#f"); },
            SexpValue::Char(c) => print!("#\\{}", c),
            SexpValue::Vector(ref v) => {
                print!("#");
                self.print_vec(symtbl, v);
            },
            SexpValue::List(ref l) => self.print_vec(symtbl, l)
        }
    }

    fn print_vec(&self, symtbl: &SymbolTable, v: &Vec<Sexp>) {
        print!("(");
        let mut it = v.iter();
        if let Some(s) = it.next() {
            s.print(symtbl);
        }
        for s in it {
            print!(" ");
            s.print(symtbl);
        }
        print!(")");
    }

    pub fn formatter<'a>(&'a self, symtbl: &'a SymbolTable) -> SexpFormatter<'a> {
        SexpFormatter { s: self, symtbl: symtbl }
    }

    // get a reference to the first sexp in a list or vector
    pub fn first(&self) -> Option<&Sexp> {
        match self.value {
            SexpValue::List(ref l) => {
                if l.len() >= 1 {
                    Some(&l[0])
                } else {
                    None
                }                    
            },
            SexpValue::Vector(ref v) => {
                if v.len() >= 1 {
                    Some(&v[0])
                } else {
                    None
                }
            },
            _ => None
        }
    }
}

#[test]
fn test_first() {
    let mut symtbl = SymbolTable::new();
    let s = Reader::new(String::from("(define a 42)")).read_sexp(&mut symtbl).unwrap();

    assert_eq!(s.first().unwrap().value, SexpValue::Symbol(symtbl.intern_str("define")));

    let s2 = Reader::new(String::from("alpaca")).read_sexp(&mut symtbl).unwrap();
    assert_eq!(s2.first(), None);
}

impl<'a> SexpFormatter<'a> {
    fn fmt_vector(&self, f: &mut fmt::Formatter, v: &Vec<Sexp>) -> fmt::Result {
        write!(f, "(").and_then(|_| {
            let mut it = v.iter();
            if let Some(s) = it.next() {
                self.fmt_sexp(f, s).expect("fmt_vector");
            }
            for s in it {
                write!(f, " ").expect("fmt_vector");
                self.fmt_sexp(f, s).expect("fmt_vector");
            }
            write!(f, ")")
        })
    }

    fn fmt_sexp(&self, f: &mut fmt::Formatter, s: &Sexp) -> fmt::Result {
        match s.value {
            SexpValue::Number(ref n) => write!(f, "{}", n),
            SexpValue::Str(ref s) => write!(f, "\"{}\"", s),
            SexpValue::Symbol(sh) => self.symtbl.display_symbol(sh, f),
            SexpValue::Bool(b) => if b { write!(f, "#t") } else { write!(f, "#f") },
            SexpValue::Char(c) => write!(f, "#\\{}", c),
            SexpValue::Vector(ref v) => write!(f, "#").and_then(|_| self.fmt_vector(f, v) ),
            SexpValue::List(ref l) => self.fmt_vector(f, l)
        }
    }
}

impl<'a> fmt::Display for SexpFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_sexp(f, self.s)
    }
}

pub struct Reader {
    lexer: Lexer
}

#[derive(PartialEq, Eq, Debug)]
pub enum ReadErrorKind {
    Eof,
    UnclosedList,
    NotImplemented,
    UnexpectedRParen,
    Lexer(LexErrorKind)
}

#[derive(PartialEq, Eq, Debug)]
pub struct ReadError {
    loc: Loc,
    kind: ReadErrorKind
}

impl ReadError {
    fn from_lexer_error(lerr: LexError) -> ReadError {
        ReadError { loc: lerr.loc, kind: ReadErrorKind::Lexer(lerr.kind) }
    }
}

type ReadResult = Result<Sexp, ReadError>;

impl Reader {
    pub fn new(s: String) -> Reader {
        Reader { lexer: Lexer::new(s) }
    }

    fn wrap_sexp(&mut self, symtbl: &mut SymbolTable,
                 sloc: Loc, eloc: Loc, s: &str) -> ReadResult {
        self.read_sexp(symtbl).and_then(|inner| {
            let mut v : Vec<Sexp> = Vec::new();
            v.push(Sexp::new(sloc.clone(), eloc, SexpValue::Symbol(symtbl.intern_str(s))));
            let eqloc = inner.end.clone();
            v.push(inner);
            Ok(Sexp::new(sloc, eqloc, SexpValue::List(v)))
        })
    }

    pub fn read_sexp(&mut self, symtbl: &mut SymbolTable) -> ReadResult {
        match self.lexer.next_token(symtbl) {
            Err(lerr) => Err(ReadError::from_lexer_error(lerr)),
            Ok(tok) => self.process_token(symtbl, tok)
        }
    }

    fn process_token(&mut self, symtbl: &mut SymbolTable, tok: Token) -> ReadResult {
        match tok.value {
            TokenValue::Eof => Err(ReadError { loc: self.lexer.current_loc(), kind: ReadErrorKind::Eof }),
            TokenValue::Number(n) => Ok(Sexp::new(tok.start, tok.end, SexpValue::Number(n))),
            TokenValue::Str(s) => Ok(Sexp::new(tok.start, tok.end, SexpValue::Str(s))),
            TokenValue::Bool(b) => Ok(Sexp::new(tok.start, tok.end, SexpValue::Bool(b))),
            TokenValue::Symbol(s) => Ok(Sexp::new(tok.start, tok.end, SexpValue::Symbol(s))),
            TokenValue::Quote => self.wrap_sexp(symtbl, tok.start, tok.end, "quote"),
            TokenValue::Comma => self.wrap_sexp(symtbl, tok.start, tok.end, "unquote"),
            TokenValue::Backquote => self.wrap_sexp(symtbl, tok.start, tok.end, "quasiquote"),
            TokenValue::LParen => self.read_list(symtbl, tok.start),
            TokenValue::RParen => Err(ReadError { loc: self.lexer.current_loc(), kind: ReadErrorKind::UnexpectedRParen }),
            TokenValue::HashParen => self.read_vector(symtbl, tok.start),
        }
    }

    fn read_sexps(&mut self, symtbl: &mut SymbolTable) -> Result<Vec<Sexp>, ReadError> {
        let mut done = false;
        let mut v : Vec<Sexp> = Vec::new();

        while !done {
            let tok_err = self.lexer.next_token(symtbl);

            if let Err(lerr) = tok_err {
                return Err(ReadError::from_lexer_error(lerr));
            }

            let tok = tok_err.unwrap();

            match tok.value {
                TokenValue::RParen => { done = true; },
                _ => {
                    let sexp_err = self.process_token(symtbl, tok);

                    if let Err(e) = sexp_err {
                        return Err(e);
                    }

                    v.push(sexp_err.unwrap());
                }
            }
        }

        Ok(v)
    }

    fn read_list(&mut self, symtbl: &mut SymbolTable, sloc: Loc) -> ReadResult {
        self.read_sexps(symtbl)
            .and_then(|v| Ok(Sexp::new(sloc, self.lexer.current_loc(),
                                       SexpValue::List(v))))
    }

    fn read_vector(&mut self, symtbl: &mut SymbolTable, sloc: Loc) -> ReadResult {
        self.read_sexps(symtbl)
            .and_then(|v| Ok(Sexp::new(sloc, self.lexer.current_loc(),
                                       SexpValue::Vector(v))))
    }
}
