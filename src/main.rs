pub mod parser;

pub struct SchemeObject {

}

pub enum SchemeVal {
    Symbol(String),
    Number(i64)
}

fn main() {
    println!("Hello, world!");
}
