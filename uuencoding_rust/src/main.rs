extern crate getopts;
use getopts::Options;

use std::env;
use std::fs;
use std::io;
use std::os::unix::fs::OpenOptionsExt;
use std::process;

extern crate uue;

#[derive(Debug)]
enum Mode {
    Encode,
    Decode,
}

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

fn split_args(args: Vec<String>) -> Result<(Mode, String), CmdError> {
    let exec = &args[0];
    let args = &args[1..];

    let mut opts = Options::new();
    opts.optflag("e", "", "encode input file");
    opts.optflag("d", "", "decode input file");
    opts.optopt("i", "input", "input file path", "PATH");

    let usage = opts.usage(&format!("Usage: {} -d|-e -i PATH", exec));
    let make_err = |desc: &str| -> Result<(Mode, String), CmdError> {
        Err(CmdError::SplitArgs(format!("{}\n\n{}", desc, usage)))
    };

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

fn _main() -> Result<(), CmdError> {
    let args = env::args().collect();
    let (mode, path) = try!(split_args(args));
    match mode {
        Mode::Encode => try!(encode_file(&path)),
        Mode::Decode => try!(decode_file(&path)),
    };
    Ok(())
}

fn main() {
    match _main() {
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
        _ => {}
    }
}
