// REPL

use std::io;
use std::io::BufRead;
use std::io::Write;

use reader::Reader;
use symtbl::SymbolTable;

use eval::State;

pub fn repl() {
    let mut done = false;

    let mut symtbl = SymbolTable::new();
    let mut state = State::new(1000);
    
    while !done {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        let stdin = io::stdin();
        stdin.lock().read_line(&mut line).expect("could not read line");

        if line.trim() == "(exit)" { done = true; }
        
        let mut r = Reader::new(line);
        let ms = r.read_sexp(&mut symtbl);

        if let Ok(sxp) = ms {
            println!("{}", sxp.formatter(&symtbl));
            println!("first: {:?}", sxp.first());
        } else {
            println!("{:?}", ms);
        }
    }
}
