// evaluation

use std::collections::HashMap;

use symtbl::SymbolHandle;

use reader::Sexp;
use reader::SexpValue;

use mem::Cell;
use mem::CellValue;

struct Env {
    bindings: HashMap<SymbolHandle, mem::Link>
}

impl Env {
    pub fn empty() -> Env {
        Env { bindings: HashMap::new() }
    }
}

struct State {
    mem: Memory,
    global_env: Env
}

impl State {
    pub fn new(memsize: usize) -> State {
        State { mem: Memory::new(memsize), global_env: Env::empty() }
    }
}

enum EvalErrorKind {
    UnboundVariable
}

enum EvalResult {
    Void,
    Value(mem::Link),
    Error(EvalErrorKind)
}

fn value(l: mem::Link) -> EvalResult {
    EvalResult::Value(l)
}

fn sexp_to_cell_value(s: &Sexp, symtbl: &SymbolTable, state: &mut State) -> CellValue {
}

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
