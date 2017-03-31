#[derive(Debug, PartialEq)]
pub enum Item {
    Symbol(char),
    Group(Regexp),
    Range(Vec<char>),
}

#[derive(Debug, PartialEq)]
pub struct Regexp(pub Vec<Item>);
