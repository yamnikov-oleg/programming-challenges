use std::error::Error;

#[derive(Debug, PartialEq)]
pub enum Token {
    Symbol(char),
    StartRange,
    RangeHyphen,
    EndRange,
    StartGroup,
    EndGroup,
}

enum State {
    ExpectSymbol,
    ExpectSpecialSymbol,
    ParseRange,
    ExpectSpecialSymbolRange,
}

pub fn split_into_tokens(regexp: &str) -> Result<Vec<Token>, Box<Error>> {
    let mut state = State::ExpectSymbol;
    let mut tokens = Vec::new();

    for ch in regexp.chars() {
        match state {
            State::ExpectSymbol => {
                match ch {
                    '\\' => state = State::ExpectSpecialSymbol,
                    '(' => tokens.push(Token::StartGroup),
                    ')' => tokens.push(Token::EndGroup),
                    '[' => {
                        tokens.push(Token::StartRange);
                        state = State::ParseRange;
                    }
                    ch => tokens.push(Token::Symbol(ch)),
                }
            }
            State::ExpectSpecialSymbol => {
                tokens.push(Token::Symbol(ch));
                state = State::ExpectSymbol;
            }
            State::ParseRange => {
                match ch {
                    ']' => {
                        tokens.push(Token::EndRange);
                        state = State::ExpectSymbol;
                    }
                    '-' => tokens.push(Token::RangeHyphen),
                    '\\' => state = State::ExpectSpecialSymbolRange,
                    ch => tokens.push(Token::Symbol(ch)),
                }
            }
            State::ExpectSpecialSymbolRange => {
                tokens.push(Token::Symbol(ch));
                state = State::ParseRange;
            }
        }
    }

    Ok(tokens)
}

#[cfg(test)]
macro_rules! assert_split {
    ($input:expr, $expect:expr) => (
        let actual = split_into_tokens($input).unwrap();
        assert_eq!($expect, actual);
    );
}

#[test]
fn test_split_basic() {
    let input = "abc";
    let expect = vec![Token::Symbol('a'), Token::Symbol('b'), Token::Symbol('c')];
    assert_split!(input, expect);
}

#[test]
fn test_split_group() {
    let input = r"a(b\)c)";
    let expect = vec![Token::Symbol('a'),
                      Token::StartGroup,
                      Token::Symbol('b'),
                      Token::Symbol(')'),
                      Token::Symbol('c'),
                      Token::EndGroup];
    assert_split!(input, expect);
}

#[test]
fn test_split_range() {
    let input = r"a[c-z\]()]";
    let expect = vec![Token::Symbol('a'),
                      Token::StartRange,
                      Token::Symbol('c'),
                      Token::RangeHyphen,
                      Token::Symbol('z'),
                      Token::Symbol(']'),
                      Token::Symbol('('),
                      Token::Symbol(')'),
                      Token::EndRange];
    assert_split!(input, expect);
}
