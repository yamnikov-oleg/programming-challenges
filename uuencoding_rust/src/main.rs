extern crate getopts;
use getopts::Options;

use std::env;
use std::fs;
use std::process;

extern crate uue;

#[derive(Debug)]
enum Mode {
    Encode,
    Decode,
}

fn split_args(args: Vec<String>) -> Result<(Mode, String), String> {
    let exec = &args[0];
    let args = &args[1..];

    let mut opts = Options::new();
    opts.optflag("e", "", "encode input file");
    opts.optflag("d", "", "decode input file");
    opts.optopt("i", "input", "input file path", "PATH");

    let usage = opts.usage(&format!("Usage: {} -d|-e -i PATH", exec));
    let make_err =
        |desc: &str| -> Result<(Mode, String), String> { Err(format!("{}\n\n{}", desc, usage)) };

    let matches: getopts::Matches = opts.parse(args).unwrap();

    let enc = matches.opt_present("e");
    let dec = matches.opt_present("d");
    let mode = match (enc, dec) {
        (true, true) => return make_err("Flags -e and -d can't be present both"),
        (false, false) => return make_err("Specify at least one flag: -e or -d"),
        (true, _) => Mode::Encode,
        (_, true) => Mode::Decode,
    };

    let fname = match matches.opt_str("i") {
        Some(fname) => fname,
        None => return make_err("Please, specify input file with -i option."),
    };

    Ok((mode, fname))
}

fn main() {
    let args = env::args().collect();

    let (_, filename) = split_args(args).unwrap_or_else(|err| {
        println!("{}", err);
        process::exit(1);
    });

    let enc = match fs::File::open(&filename) {
        Ok(f) => uue::Encoder::from_file(filename.clone(), f),
        Err(e) => {
            println!("Error when opening {}:\n{}", filename, e);
            process::exit(1);
        }
    };

    let mut out = fs::File::create(&format!("{}.uu", filename)).unwrap();
    enc.encode_to(&mut out).unwrap();
}
