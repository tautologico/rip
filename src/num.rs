
#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Num {
    Inexact(i64), // TODO floating point
    Exact(i64)    // TODO BigRational
}

impl Num {
    pub fn print(&self) {
        match self {
            &Num::Inexact(i) => print!("{}", i),
            &Num::Exact(e) => print!("{}", e)
        }
    }
}
