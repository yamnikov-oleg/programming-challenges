extern crate getopts;
use getopts::Options;

use std::env;
use std::fs;
use std::io;
use std::os::unix::fs::OpenOptionsExt;
use std::process;

extern crate uue;

enum CmdError {
    SplitArgs(String),
    Io(io::Error),
    Decode(uue::Error),
}

impl From<String> for CmdError {
    fn from(s: String) -> CmdError {
        CmdError::SplitArgs(s)
    }
}

impl From<io::Error> for CmdError {
    fn from(e: io::Error) -> CmdError {
        CmdError::Io(e)
    }
}

impl From<uue::Error> for CmdError {
    fn from(e: uue::Error) -> CmdError {
        CmdError::Decode(e)
    }
}

impl std::fmt::Display for CmdError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &CmdError::SplitArgs(ref s) => {
                try!(write!(f, "{}", s));
            }
            &CmdError::Io(ref e) => {
                try!(write!(f, "{}", e));
            }
            &CmdError::Decode(ref e) => {
                try!(write!(f, "{}", e));
            }
        };
        Ok(())
    }
}

struct Args {
    to_encode: Vec<String>,
    to_decode: Vec<String>,
}

fn split_args(args: Vec<String>) -> Args {
    let exec = &args[0];
    let args = &args[1..];

    let mut opts = Options::new();
    opts.optmulti("e", "encode", "encode a file", "PATH");
    opts.optmulti("d", "decode", "decode a file", "PATH");

    let matches: getopts::Matches = opts.parse(args).unwrap();
    Args {
        to_encode: matches.opt_strs("encode"),
        to_decode: matches.opt_strs("decode"),
    }
}

fn encode_file(path: &str) -> Result<(), CmdError> {
    let infile = try!(fs::File::open(path));
    let enc = uue::Encoder::from_file(path.to_string(), infile);

    let mut out = try!(fs::File::create(&format!("{}.uu", path)));

    try!(enc.encode_to(&mut out));
    Ok(())
}

fn decode_file(path: &str) -> Result<(), CmdError> {
    let mut infile = try!(fs::File::open(path));
    let mut dec = uue::Decoder::new(&mut infile);

    let (perm, fname): (u32, String) = try!(dec.head_ref()
        .map(|(p, f)| (p.to_owned(), f.to_owned())));

    let mut options = fs::OpenOptions::new();
    let mut out = try!(options.write(true).create(true).mode(perm).open(fname));

    try!(dec.decode_to(&mut out));
    Ok(())
}

fn run() -> Result<(), CmdError> {
    let args = split_args(env::args().collect());
    for path in args.to_encode {
        try!(encode_file(&path));
    }
    for path in args.to_decode {
        try!(decode_file(&path))
    }
    Ok(())
}

fn main() {
    match run() {
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
        _ => {}
    }
}
