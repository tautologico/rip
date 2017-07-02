// evaluation

use std::collections::HashMap;

use symtbl::SymbolHandle;

struct Env {
    bindings: HashMap<SymbolHandle, mem::Link>
}
    
struct State {
    global_env: Env
}

// pub fn eval() -> ? {
// }
