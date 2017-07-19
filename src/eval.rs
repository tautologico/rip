// evaluation

use std::collections::HashMap;

use symtbl::SymbolHandle;
use symtbl::SymbolTable;

use reader::Sexp;
use reader::SexpValue;

use mem::Link;
use mem::Cell;
use mem::CellValue;
use mem::Memory;

struct Env {
    bindings: HashMap<SymbolHandle, Link>
}

impl Env {
    pub fn empty() -> Env {
        Env { bindings: HashMap::new() }
    }
}

pub struct State {
    mem: Memory,
    global_env: Env
}

impl State {
    pub fn new(memsize: usize) -> State {
        State { mem: Memory::initialize(memsize), global_env: Env::empty() }
    }
}

pub enum EvalErrorKind {
    UnboundVariable,
    NotImplemented
}

pub enum EvalResult {
    Void,
    Value(Link),
    Error(EvalErrorKind)
}

fn value(l: Link) -> EvalResult {
    EvalResult::Value(l)
}

// fn sexp_to_cell_value(s: &Sexp, symtbl: &SymbolTable, state: &mut State) -> CellValue {
//     match s.value {
//         SexpValue::Bool(b) => value(state.mem.alloc(CellValue::Boolean(b))),
//         SexpValue::Symbol(h) => value(state.mem.alloc(CellValue::Symbol(h))),
//         SexpValue::Number(ref n) => value(state.mem.alloc(CellValue::Number(n.clone()))),
//         SexpValue::Str(ref s) => value(state.mem.alloc(CellValue::Str(s.clone()))),
//         SexpValue::Char(c) => value(state.mem.alloc(CellValue::Char(c))),
//         SexpValue::Vector(ref v) => value(state.mem.alloc(CellValue::Vector(v.map(|s| sexp_to_cell_value(s, symtbl, state))))),
//         SexpValue::List(ref l) => CellValue::Nil
//     }
// }

pub fn eval(s: &Sexp, symtbl: &SymbolTable, state: &mut State) -> EvalResult {
    match s.value {
        SexpValue::Bool(b) => value(state.mem.alloc(CellValue::Boolean(b))),
        SexpValue::Symbol(h) => value(state.mem.alloc(CellValue::Symbol(h))),
        SexpValue::Number(ref n) => value(state.mem.alloc(CellValue::Number(n.clone()))),
        SexpValue::Str(ref s) => value(state.mem.alloc(CellValue::Str(s.clone()))),
        SexpValue::Char(c) => value(state.mem.alloc(CellValue::Char(c))),
        _ => EvalResult::Void
    }
}

// TODO eval sexp s as a quoted sexp
fn eval_quoted(s: &Sexp, symtbl: &SymbolTable, state: &State) -> Link {
    Some(0)
}
