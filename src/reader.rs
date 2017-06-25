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

pub struct Reader {
    lexer: Lexer
}

pub enum ReadErrorKind {
    UnclosedList,
    Lexer(LexErrorKind)
}

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

    pub fn read_sexp(&mut self, symtbl: &mut SymbolTable) -> ReadResult {
        match self.lexer.next_token(symtbl) {
            Err(lerr) => Err(ReadError::from_lexer_error(lerr)),
            Ok(tok) => {
                match tok.value {
                    TokenValue::Number(n) => Ok(Sexp { start: tok.start, end: tok.end, value: SexpValue::Number(n)}),
                    // TODO
                }
            }
        }
    }
}
//pub fn read_sexp()