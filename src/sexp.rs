
pub type SymbolHandle = usize;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Sexp {
    Number(i64),
    Str(String),
    Symbol(String),
    Bool(bool),
    List(Vec<Sexp>)
}
