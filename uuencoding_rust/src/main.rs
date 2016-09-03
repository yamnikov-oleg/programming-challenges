extern crate getopts;
use getopts::Options;

use std::env;
use std::fs;
use std::os::unix::fs::OpenOptionsExt;
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

fn encode_file(path: &str) {
    let infile = fs::File::open(path).unwrap();
    let enc = uue::Encoder::from_file(path.to_string(), infile);

    let mut out = fs::File::create(&format!("{}.uu", path)).unwrap();

    enc.encode_to(&mut out).unwrap();
}

fn decode_file(path: &str) {
    let mut infile = fs::File::open(path).unwrap();
    let mut dec = uue::Decoder::new(&mut infile);

    let (perm, fname): (u32, String) =
        dec.head_ref().map(|(p, f)| (p.to_owned(), f.to_owned())).unwrap();

    let mut options = fs::OpenOptions::new();
    let mut out = options.write(true).create(true).mode(perm).open(fname).unwrap();

    dec.decode_to(&mut out).unwrap();
}

fn main() {
    let args = env::args().collect();
    let (mode, path) = split_args(args).unwrap();
    match mode {
        Mode::Encode => encode_file(&path),
        Mode::Decode => decode_file(&path),
    }
}
