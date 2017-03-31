extern crate clap;

mod regexp;
mod tokens;
mod syntax;

use clap::{Arg, App};

fn print_re(re: &regexp::Regexp) {
    use regexp::Item;
    for item in re.0.iter() {
        match *item {
            Item::Symbol(ch) => print!("{} > ", ch),
            Item::Range(ref rng) => {
                print!("[ ");
                for ch in rng {
                    print!("{} | ", ch);
                }
                print!("$ ] > ");
            }
            Item::Group(ref inner) => {
                print!("( ");
                print_re(inner);
                print!(") ");
            }
        }
    }
    print!("$ ");
}

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
    let ast = syntax::parse(&tokens).unwrap();
    print_re(&ast);
    println!();
}
