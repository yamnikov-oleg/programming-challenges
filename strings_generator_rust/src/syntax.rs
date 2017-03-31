use std::ops::IndexMut;

use regexp::{Regexp, Item};
use tokens::Token;

#[derive(Debug)]
pub enum Error {
    UnexpectedEndOfStream,
    UnexpectedToken(Token),
}

impl ::std::fmt::Display for Error {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Error::UnexpectedEndOfStream => write!(f, "Unexpected end of stream"),
            Error::UnexpectedToken(ref tok) => write!(f, "Unexpected token {:?}", tok),
        }
    }
}

impl ::std::error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::UnexpectedEndOfStream => "Unexpected end of stream",
            Error::UnexpectedToken(_) => "Unexpected token",
        }
    }
}

enum State {
    ParsingSequence,
    ParsingRange,
    AfterRangeHyphen,
    ParsingGroup,
    Error(Error),
}

struct Parser {
    state: State,
    re_stack: Vec<Item>,
    group: Option<Box<Parser>>,
}

impl Parser {
    fn new() -> Parser {
        Parser {
            state: State::ParsingSequence,
            re_stack: Vec::new(),
            group: None,
        }
    }

    fn last_item_mut(&mut self) -> &mut Item {
        let len = self.re_stack.len();
        self.re_stack.index_mut(len - 1)
    }

    fn last_item_range_mut(&mut self) -> &mut Vec<char> {
        match *self.last_item_mut() {
            Item::Range(ref mut range) => range,
            _ => panic!("last_item_range_mut was called when last item was not range"),
        }
    }

    fn execute(&mut self, token: &Token) {
        match self.state {
            State::Error(_) => return,
            State::ParsingSequence => {
                match *token {
                    Token::Symbol(ch) => self.re_stack.push(Item::Symbol(ch)),
                    Token::StartRange => {
                        self.re_stack.push(Item::Range(Vec::new()));
                        self.state = State::ParsingRange;
                    }
                    Token::StartGroup => {
                        self.group = Some(Box::new(Parser::new()));
                        self.state = State::ParsingGroup;
                    }
                    ref tok => self.state = State::Error(Error::UnexpectedToken(tok.clone())),
                }
            }
            State::ParsingRange => {
                match *token {
                    Token::Symbol(ch) => self.last_item_range_mut().push(ch),
                    Token::EndRange => self.state = State::ParsingSequence,
                    Token::RangeHyphen => self.state = State::AfterRangeHyphen,
                    ref tok => self.state = State::Error(Error::UnexpectedToken(tok.clone())),
                }
            }
            State::AfterRangeHyphen => {
                match *token {
                    Token::Symbol(ch) => {
                        {
                            let last_range = self.last_item_range_mut();
                            let last_char = last_range[last_range.len() - 1];

                            let range_u32 = (last_char as u32 + 1)..(ch as u32 + 1);
                            let range_char = range_u32.filter_map(::std::char::from_u32)
                                .collect::<Vec<char>>();
                            last_range.extend(range_char);
                        }
                        self.state = State::ParsingRange;
                    }
                    ref tok => self.state = State::Error(Error::UnexpectedToken(tok.clone())),
                }
            }
            State::ParsingGroup => {
                match *token {
                    Token::EndGroup => {
                        let group = match self.group.take().unwrap().build() {
                            Ok(g) => g,
                            Err(e) => {
                                self.state = State::Error(e);
                                return;
                            }
                        };
                        self.re_stack.push(Item::Group(group));
                        self.state = State::ParsingSequence;
                    }
                    ref tok => self.group.as_mut().map(|p| p.execute(tok)).unwrap(),
                };
            }
        }
    }

    fn build(self) -> Result<Regexp, Error> {
        match self.state {
            State::Error(e) => Err(e),
            State::ParsingSequence => Ok(Regexp(self.re_stack)),
            _ => Err(Error::UnexpectedEndOfStream),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Regexp, Error> {
    let mut parser = Parser::new();
    for tok in tokens {
        parser.execute(tok);
    }
    parser.build()
}

#[test]
fn test_simple() {
    let input = &[Token::Symbol('a'), Token::Symbol('b'), Token::Symbol('c')];
    let expect = Regexp(vec![Item::Symbol('a'), Item::Symbol('b'), Item::Symbol('c')]);
    assert_eq!(expect, parse(input).unwrap());
}

#[test]
fn test_range() {
    let input = &[Token::Symbol('a'),
                  Token::StartRange,
                  Token::Symbol('b'),
                  Token::Symbol('c'),
                  Token::EndRange,
                  Token::Symbol('d')];
    let expect = Regexp(vec![Item::Symbol('a'), Item::Range(vec!['b', 'c']), Item::Symbol('d')]);
    assert_eq!(expect, parse(input).unwrap());
}

#[test]
fn test_range_hyphen() {
    let input = &[Token::Symbol('a'),
                  Token::StartRange,
                  Token::Symbol('b'),
                  Token::RangeHyphen,
                  Token::Symbol('e'),
                  Token::Symbol('z'),
                  Token::EndRange];
    let expect = Regexp(vec![Item::Symbol('a'), Item::Range(vec!['b', 'c', 'd', 'e', 'z'])]);
    assert_eq!(expect, parse(input).unwrap());
}

#[test]
fn test_group() {
    let input = &[Token::Symbol('a'),
                  Token::StartGroup,
                  Token::Symbol('b'),
                  Token::Symbol('c'),
                  Token::EndGroup,
                  Token::Symbol('d')];
    let expect = Regexp(vec![Item::Symbol('a'),
                             Item::Group(Regexp(vec![Item::Symbol('b'), Item::Symbol('c')])),
                             Item::Symbol('d')]);
    assert_eq!(expect, parse(input).unwrap());
}
