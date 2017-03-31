extern crate clap;

mod tokens;

use clap::{Arg, App};

fn main() {
    let matches = App::new("Strings Generator")
        .author(env!("CARGO_PKG_AUTHORS"))
        .version(env!("CARGO_PKG_VERSION"))
        .args(&[Arg::with_name("REGEXP")
                    .help("Regular expression to generate strings from")
                    .required(true)])
        .get_matches();

    let regexp = matches.value_of("REGEXP").unwrap();

    let tokens = tokens::split_into_tokens(&regexp).unwrap();
    use tokens::Token;
    for t in tokens {
        match t {
            Token::Symbol(ch) => println!("Symbol {}", ch),
            Token::StartGroup => println!("( StartGroup"),
            Token::EndGroup => println!(") EndGroup"),
            Token::StartRange => println!("[ StartRange"),
            Token::EndRange => println!("] EndRange"),
            Token::RangeHyphen => println!("- RangeHyphen"),
        }
    }
}
