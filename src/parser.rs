use sexp::Sexp;

pub struct ParserState {
    chars: Vec<char>,
    ix: usize
}

impl ParserState {
    pub fn new(s: String) -> Self {
        ParserState { chars: s.chars().collect(), ix: 0 }
    }

    pub fn peek(&self) -> char {
        self.chars[self.ix]
    }

    pub fn next(&mut self) -> char {
        let res = self.chars[self.ix];
        self.ix += 1;
        res
    }
}

#[test]
fn test_peek() {
    let ps1 = ParserState::new(String::from("Aussonderungsaxiom"));

    assert_eq!(ps1.peek(), 'A');
}

#[test]
fn test_next() {
    let mut ps1 = ParserState::new(String::from("Aussonderungsaxiom"));

    assert_eq!(ps1.next(), 'A');
    assert_eq!(ps1.next(), 'u');
    assert_eq!(ps1.next(), 's');
    assert_eq!(ps1.peek(), 's');
}

pub fn parse_atom(ps: &mut ParserState) -> Sexp {
    match ps.peek() {
        '#' => parse_hash(ps),
        '"' => parse_string(ps),
        '\'' => quoted_sexp()
    }
}

// parses a top-level sexp (a list)
// assumes the opening parenthesis was already consumed
pub fn parse_sexp(ps: &mut ParserState) -> Sexp {
    let mut res : Vec<Sexp> = Vec::new();

    while ps.peek() != ')' {
        match ps.peek() {
            '(' => { ps.next(); res.push(parse_sexp(ps)) },
            _ => res.push(parse_atom(ps))
        }
    }

    Sexp::List(res)
}
