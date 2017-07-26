// memory representation

use num::Num;

use symtbl::SymbolHandle;

pub type Link = Option<usize>;
pub type RawLink = usize;

pub enum CellValue {
    Empty(Link),    // an empty cell links to other empty cells (free list)
    Nil,
    Number(Num),
    Boolean(bool),
    Str(String),
    Cons(Cons),
    Vector(Vec<RawLink>),  // Vector(Vec<CellValue>),
    Char(char),
    Symbol(SymbolHandle),
    Closure
}

impl CellValue {
    fn cons(head: Link, tail: Link) -> CellValue {
        CellValue::Cons(Cons { car: head, cdr: tail })
    }
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

pub struct Memory {
    cells: Vec<Cell>,
    available: usize,
    free: Link        // head of free list
}

pub const NIL : Link = Some(0);

impl Memory {
    pub fn initialize(size: usize) -> Memory {
        let mut res = Memory { cells: Vec::with_capacity(size),
                               available: size,
                               free: Some(1) };

        // push the Nil cell
        res.cells.push(Cell { value: CellValue::Nil, mark: false });

        // initialize free cells and the free list
        for i in 1..size-1 {
            res.cells.push(Cell { value: CellValue::Empty(Some(i+1)), mark: false });
        }

        // add the last cell in the free list
        res.cells.push(Cell { value: CellValue::Empty(None), mark: false });

        res
    }

    pub fn available(&self) -> usize {
        self.available
    }

    fn alloc_cell(&mut self, ix: usize, val: CellValue) -> Link {
        let mut c = &mut self.cells[ix];

        if let CellValue::Empty(p) = c.value {
            self.free = p;
            self.available -=1;
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

    pub fn vector_ref(&self, lnk: RawLink) -> &Cell {
        &self.cells[lnk]
    }

    pub fn set_cell(&mut self, lnk: Link, c: Cell) {
        if let Some(ix) = lnk {
            self.cells[ix] = c;
        }
    }

    pub fn alloc_vec(&mut self, vals: Vec<CellValue>) -> CellValue {
        if vals.len() > self.available {
            self.collect_garbage();
            if vals.len() > self.available {
                panic!("No memory available for vector of size {}", vals.len())
            }
        }

        let mut res : Vec<RawLink> = Vec::new();
        for val in vals {
            let link = self.alloc(val);
            res.push(link.unwrap());  // alloc always returns a valid link (or doesn't return)
        }
        CellValue::Vector(res)
    }

    pub fn singleton_list(&mut self, val: CellValue) -> Link {
        let vallink = self.alloc(val);
        self.alloc(CellValue::cons(vallink, NIL))
    }

    pub fn cons_val(&mut self, head: CellValue, tail: Link) -> Link {
        let headlnk = self.alloc(head);
        self.alloc(CellValue::cons(headlnk, tail))
    }

    pub fn cons(&mut self, head: Link, tail: Link) -> Link {
        self.alloc(CellValue::cons(head, tail))
    }
}
