
#[test]
fn test() {
    println!("It works!");
}

struct ParserState {
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
