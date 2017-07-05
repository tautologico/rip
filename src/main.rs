pub mod sexp;
pub mod num;
pub mod symtbl;
pub mod lexer;
pub mod reader;
pub mod mem;

use std::io::BufRead;
use std::io::Write;

fn main() {
    print!("sexp: ");
    std::io::stdout().flush().unwrap();
    let mut line = String::new();
    let stdin = std::io::stdin();
    stdin.lock().read_line(&mut line).expect("could not read line");

    let mut symtbl = symtbl::SymbolTable::new();
    let mut r = reader::Reader::new(line);
    let s = r.read_sexp(&mut symtbl);
    println!("{:?}", s);

    if let Ok(sxp) = s {
        print!("sexp: ");
        sxp.print(&symtbl);
        println!("");

        println!("sexp: {}", sxp.formatter(&symtbl));
    }
}
