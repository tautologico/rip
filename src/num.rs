use std::fmt;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Num {
    Inexact(i64), // TODO floating point
    Exact(i64)    // TODO BigRational
}

impl fmt::Display for Num {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Num::Inexact(i) => write!(f, "{}", i),
            &Num::Exact(e) => write!(f, "{}", e)
        }
    }
}

impl Num {
    pub fn print(&self) {
        match self {
            &Num::Inexact(i) => print!("{}", i),
            &Num::Exact(e) => print!("{}", e)
        }
    }
}
