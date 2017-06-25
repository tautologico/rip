use lexer::Lexer;
use lexer::LexError;
use lexer::LexErrorKind;
use lexer::Loc;
use lexer::Token;
use lexer::TokenValue;

use num::Num;

use symtbl::SymbolTable;
use symtbl::SymbolHandle;

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

impl Sexp {
    fn new(start: Loc, end: Loc, val: SexpValue) -> Sexp {
        Sexp { start: start, end: end, value: val }
    }
}

pub struct Reader {
    lexer: Lexer
}

#[derive(PartialEq, Eq, Debug)]
pub enum ReadErrorKind {
    UnclosedList,
    NotImplemented,
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
            Ok(tok) => {
                match tok.value {
                    TokenValue::Number(n) => Ok(Sexp::new(tok.start, tok.end, SexpValue::Number(n))),
                    TokenValue::Str(s) => Ok(Sexp::new(tok.start, tok.end, SexpValue::Str(s))),
                    TokenValue::Bool(b) => Ok(Sexp::new(tok.start, tok.end, SexpValue::Bool(b))),
                    TokenValue::Symbol(s) => Ok(Sexp::new(tok.start, tok.end, SexpValue::Symbol(s))),
                    TokenValue::Quote => self.wrap_sexp(symtbl, tok.start, tok.end, "quote"),
                    TokenValue::Comma => self.wrap_sexp(symtbl, tok.start, tok.end, "unquote"),
                    TokenValue::Backquote => self.wrap_sexp(symtbl, tok.start, tok.end, "quasiquote"),
                    TokenValue::LParen => self.read_list(symtbl, tok.start),
                    _ => Err(ReadError { loc: self.lexer.current_loc(), kind: ReadErrorKind::NotImplemented })
                }
            }
        }
    }

    fn token_to_sexp(&mut self, symtbl: &mut SymbolTable, tok: Token) -> ReadResult {
        // TODO
    }

    fn read_list(&mut self, symtbl: &mut SymbolTable, sloc: Loc) -> ReadResult {
        let mut bool done = false;
        let mut v : Vec<Sexp> = Vec::new();

        while !done {
            match self.lexer.next_token(symtbl) {
                Err(lerr) => return Err(ReadError::from_lexer_error(lerr)),
                Ok(tok) => match tok.value {
                    // TODO
                }
            }
        }
    }
}
