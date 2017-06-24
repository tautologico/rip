
#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Num {
    Inexact(i64), // TODO floating point
    Exact(i64)    // TODO BigRational
}

