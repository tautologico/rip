// memory representation

use num::Num;

use symtbl::SymbolHandle;

type Link = Option<usize>;

enum CellValue {
    Empty(Link),    // an empty cell links to other empty cells (free list)
    Nil,
    Number(Num),
    Boolean(bool),
    Str(String),
    Cons(Cons),
    Vector(Vec<CellValue>),
    Char(char),
    Symbol(SymbolHandle),
    Closure
}

// A memory cell
pub struct Cell {
    value: CellValue,
    mark: bool
}

pub struct Cons {
    car: Link,
    cdr: Link
}

struct Memory {
    cells: Vec<Cell>,
    free: Link        // head of free list
}


impl Memory {
    pub fn initialize(size: usize) -> Memory {
        let mut res = Memory { cells: Vec::with_capacity(size), free: Some(0) };

        // initialize free cells and the free list
        for i in 0..size-1 {
            res.cells.push(Cell { value: CellValue::Empty(Some(i+1)), mark: false });
        }

        // add the last cell in the free list
        res.cells.push(Cell { value: CellValue::Empty(None), mark: false });

        res
    }

    fn alloc_cell(&mut self, ix: usize, val: CellValue) -> Link {
        let mut c = &mut self.cells[ix];
        
        if let CellValue::Empty(p) = c.value {
            self.free = p;
        } else {
            panic!("Cell on free list was not empty");
        }
        
        c.value = val;
        Some(ix)
    }

    fn collect_garbage(&mut self) {
        println!("GC...");
    }
    
    pub fn alloc(&mut self, val: CellValue) -> Link {
        match self.free {
            None => {
                self.collect_garbage();
                if let Some(ix) = self.free {
                    self.alloc_cell(ix, val)
                } else {
                    panic!("Memory exhausted, even after GC")
                }
            },
            Some(ix) => self.alloc_cell(ix, val)
        }
    }

    // pub fn cell_ref(&self, lnk: Link) -> Option<&Cell> {
    //     lnk.and_then(|ix| Some(&self.cells[ix]))
    // }

    // pub fn cell_mut_ref(&mut self, lnk: Link) -> Option<&mut Cell> {
    //     lnk.and_then(|ix| Some(&mut self.cells[ix]))
    // }

    pub fn set_cell(&mut self, lnk: Link, c: Cell) {
        if let Some(ix) = lnk {
            self.cells[ix] = c;
        }
    }
}
