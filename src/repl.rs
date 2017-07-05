// REPL

use std::io;
use std::io::BufRead;
use std::io::Write;

use reader::Reader;
use symtbl::SymbolTable;


pub fn repl() {
    let mut done = false;

    let mut symtbl = SymbolTable::new();
    
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
