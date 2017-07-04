use std::collections::HashMap;

pub type SymbolHandle = usize;

struct SymbolInfo {
    name: String,
    handle: SymbolHandle
}

pub struct SymbolTable {
    symbols: HashMap<String, SymbolHandle>,
    info: Vec<SymbolInfo>,
    next: SymbolHandle
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable { symbols: HashMap::new(), info: Vec::new(),
                      next: 0 }
    }

    pub fn intern(&mut self, s: String) -> SymbolHandle {
        match self.symbols.get(&s) {
            Some(&h) => h,
            None => {
                let res = self.next;
                self.info.push(SymbolInfo { name: s.clone(), handle: res });
                self.symbols.insert(s, res);
                self.next += 1;
                res
            }
        }
    }

    pub fn intern_str(&mut self, s: &str) -> SymbolHandle {
        self.intern(String::from(s))
    }

    // TODO return &str
    pub fn symbol_name(&self, h: SymbolHandle) -> Option<String> {
        if self.next <= h {
            None
        } else {
            Some(self.info[h].name.clone())
        }
    }

    pub fn print_symbol(&self, h: SymbolHandle) {
        if h < self.next {
            print!("{}", self.info[h].name);
        }
    }
}

#[test]
fn test_symbol_table() {
    let mut symtbl = SymbolTable::new();

    assert_eq!(symtbl.intern_str("quote"), 0);
    assert_eq!(symtbl.intern_str("cond"), 1);
    assert_eq!(symtbl.intern_str("loop"), 2);
    assert_eq!(symtbl.intern(String::from("quote")), 0);
    assert_eq!(symtbl.intern(String::from("loop")), 2);
}
