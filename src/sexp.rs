
use symtbl::SymbolHandle;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Sexp {
    Number(i64),
    Str(String),
    Symbol(SymbolHandle),
    Bool(bool),
    Char(char),
    Vector(Vec<Sexp>),
    List(Vec<Sexp>)
}
