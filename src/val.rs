
use num::Num;

use symtbl::SymbolHandle;

pub enum SchemeValue {
    Number(Num),
    Boolean(bool),
    Vector(Vec),
    Str(String),
    Cons(mem::Cons),
    Char(char),
    Symbol(SymbolHandle),
    Closure
}
    
    
