pub mod sexp;
pub mod num;
pub mod symtbl;
pub mod lexer;
pub mod reader;
pub mod mem;
pub mod repl;

fn main() {
    repl::repl();
}
