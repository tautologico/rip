
use num::Num;

use symtbl::SymbolHandle;

pub enum SchemeValue {
    Nil,
    Number(Num),
    Boolean(bool),
    Vector(Vec),
    Str(String),
    Cons(mem::Cons),
    Char(char),
    Symbol(SymbolHandle),
    Closure
}
