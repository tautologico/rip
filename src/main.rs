pub mod sexp;
pub mod num;
pub mod symtbl;
pub mod lexer;
pub mod reader;
pub mod mem;
pub mod repl;

use std::io::BufRead;
use std::io::Write;

fn main() {
    repl::repl();
}
