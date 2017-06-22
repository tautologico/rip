pub mod sexp;
pub mod parser;

use std::io::BufRead;
use std::io::Write;

pub struct SchemeObject {

}

pub enum SchemeVal {
    Symbol(String),
    Number(i64)
}

fn main() {
    print!("sexp: ");
    std::io::stdout().flush();
    let mut line = String::new();
    let stdin = std::io::stdin();
    stdin.lock().read_line(&mut line).expect("could not read line");

    let mut symtbl = parser::SymbolTable::new();
    let mut ps = parser::ParserState::new(line);
    let s = parser::read_sexp(&mut ps, &mut symtbl);
    println!("{:?}", s);
}
