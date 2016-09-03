use std::cmp::PartialEq;
use std::io;
use std::io::Cursor;
use std::io::prelude::*;

#[derive(Debug,Eq, PartialEq)]
pub enum SyntaxErrorKind {
    UnexectedModeChar(char),
    EmptyLine,
    ShortLine,
    LongLine,
    ExpectedBegin,
    ExpectedEnd,
}

impl ::std::fmt::Display for SyntaxErrorKind {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        use SyntaxErrorKind::*;
        match self {
            &UnexectedModeChar(c) => {
                write!(f,
                       "Unexpected symbol '{}', while parsing permissions mode",
                       c)
            }
            &EmptyLine => f.write_str("Empty line"),
            &ShortLine => f.write_str("Line is too short"),
            &LongLine => f.write_str("Line is too long"),
            &ExpectedBegin => f.write_str("Expected 'begin' keyword"),
            &ExpectedEnd => f.write_str("Expected 'end' keyword"),
        }
    }
}

#[derive(Debug,Eq, PartialEq)]
pub struct SyntaxError {
    pub kind: SyntaxErrorKind,
    pub line: u32,
    pub ch: u32,
}

impl ::std::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}\nat line {} char {}", self.kind, self.line, self.ch)
    }
}

#[derive(Debug)]
pub enum Error {
    UnexpectedEof,
    Io(io::Error),
    Syntax(SyntaxError),
}

impl ::std::fmt::Display for Error {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        use Error::*;
        match self {
            &UnexpectedEof => f.write_str("Unexpected end of file"),
            &Io(ref e) => write!(f, "Error while reading/writing a file:\n{}", e),
            &Syntax(ref e) => write!(f, "{}", e),
        }
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Error) -> bool {
        match (self, other) {
            (&Error::UnexpectedEof, &Error::UnexpectedEof) => true,
            (&Error::Syntax(ref lhs), &Error::Syntax(ref rhs)) if lhs == rhs => true,
            (&Error::Io(ref lhs), &Error::Io(ref rhs)) if lhs.kind() == rhs.kind() => true,
            _ => false,
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        use std::io::ErrorKind::*;

        match err.kind() {
            UnexpectedEof => Error::UnexpectedEof,
            _ => Error::Io(err),
        }
    }
}

pub struct Decoder<T: Read> {
    pub source: T,
    line: u32,
    ch: u32,
    fname: Option<String>,
    mode: Option<u32>,
}

impl<'a> Decoder<Cursor<&'a [u8]>> {
    pub fn from_bytes(buf: &'a [u8]) -> Self {
        Decoder::new(Cursor::new(buf))
    }
}

impl Decoder<Cursor<Vec<u8>>> {
    pub fn from_vec(buf: Vec<u8>) -> Self {
        Decoder::new(Cursor::new(buf))
    }
}

impl<T: Read> Decoder<T> {
    pub fn new(src: T) -> Decoder<T> {
        Decoder {
            source: src,
            fname: None,
            mode: None,
            line: 1,
            ch: 1,
        }
    }

    fn syntax_error_here(&self, kind: SyntaxErrorKind) -> Error {
        Error::Syntax(SyntaxError {
            kind: kind,
            line: self.line,
            ch: self.ch,
        })
    }

    fn syntax_error_back(&self, kind: SyntaxErrorKind) -> Error {
        let (line, ch) = match (self.line, self.ch) {
            (l, 1) => (l - 1, 1),
            (l, c) => (l, c - 1),
        };
        Error::Syntax(SyntaxError {
            kind: kind,
            line: line,
            ch: ch,
        })
    }

    fn check_begin(&mut self) -> Result<(), Error> {
        let begin_err = self.syntax_error_here(SyntaxErrorKind::ExpectedBegin);
        let mut begin_buf = vec![0; 6];

        try!(self.read_exact(&mut begin_buf));
        if begin_buf != "begin ".as_bytes() {
            return Err(begin_err);
        }
        Ok(())
    }

    fn check_end(&mut self) -> Result<(), Error> {
        let end_err = self.syntax_error_here(SyntaxErrorKind::ExpectedEnd);
        let mut end_buf = vec![0; 4];

        try!(self.read_exact(&mut end_buf));
        if end_buf != "end\n".as_bytes() {
            return Err(end_err);
        }
        Ok(())
    }

    fn parse_mode(&mut self) -> Result<(), Error> {
        let mut mode = 0u32;
        let mut ch = [0u8; 1];

        loop {
            try!(self.read_exact(&mut ch));
            match ch[0] {
                c if b'0' <= c && c <= b'7' => {
                    mode = (mode << 3) + (c - b'0') as u32;
                }
                b' ' => break,
                c => return Err(self.syntax_error_back(SyntaxErrorKind::UnexectedModeChar(c as char))),
            }
        }
        self.mode = Some(mode);
        Ok(())
    }

    fn parse_name(&mut self) -> Result<(), Error> {
        let mut name_buf = String::new();
        let mut ch = [0u8; 1];
        loop {
            try!(self.read_exact(&mut ch));
            match ch[0] {
                b'\n' => break,
                c => name_buf.push(c as char),
            };
        }
        self.fname = Some(name_buf);
        Ok(())
    }

    fn parse_head(&mut self) -> Result<(), Error> {
        try!(self.check_begin());
        try!(self.parse_mode());
        try!(self.parse_name());
        Ok(())
    }

    pub fn head_ref(&mut self) -> Result<(&u32, &str), Error> {
        if self.fname.is_none() || self.mode.is_none() {
            try!(self.parse_head());
        }
        Ok((self.mode.as_ref().unwrap(), self.fname.as_ref().unwrap()))
    }

    fn decode_line_to<W: Write>(&mut self, mut dst: W) -> Result<usize, Error> {
        let mut line = Vec::<u8>::new();
        let mut ch = [0u8; 1];
        loop {
            try!(self.read_exact(&mut ch));
            if ch[0] == b'\n' {
                break;
            }
            line.push(ch[0]);
        }
        if line.is_empty() {
            return Err(self.syntax_error_back(SyntaxErrorKind::EmptyLine));
        }

        let decoded_len = match line[0] {
            96 => 0,
            n => (n - 32) as usize,
        };
        let chunks_count = match decoded_len % 3 {
            0 => decoded_len / 3,
            1 => (decoded_len + 2) / 3,
            2 => (decoded_len + 1) / 3,
            _ => unreachable!(),
        };
        let encoded = &mut line[1..];
        if encoded.len() < chunks_count * 4 {
            return Err(self.syntax_error_back(SyntaxErrorKind::ShortLine));
        }
        if encoded.len() > chunks_count * 4 {
            return Err(self.syntax_error_back(SyntaxErrorKind::LongLine));
        }

        fn decode_chunk(chunk: &mut [u8]) -> [u8; 3] {
            let mut ret = [0u8; 3];
            for b in chunk.iter_mut() {
                *b -= 32;
            }

            ret[0] = ((chunk[0] & 0b00111111) << 2) + ((chunk[1] & 0b00110000) >> 4);
            ret[1] = ((chunk[1] & 0b00001111) << 4) + ((chunk[2] & 0b00111100) >> 2);
            ret[2] = ((chunk[2] & 0b00000011) << 6) + ((chunk[3] & 0b00111111) >> 0);
            ret
        }

        let mut outbuf = Vec::new();
        for chunk in encoded.chunks_mut(4) {
            let dec = decode_chunk(chunk);
            outbuf.extend(dec.into_iter());
        }
        try!(dst.write_all(&outbuf[..decoded_len]));
        Ok((decoded_len))
    }

    pub fn decode_to<W: Write>(&mut self, mut dst: W) -> Result<(), Error> {
        if self.fname.is_none() || self.mode.is_none() {
            try!(self.parse_head());
        }

        loop {
            let size = try!(self.decode_line_to(&mut dst));
            if size == 0 {
                break;
            }
        }

        try!(self.check_end());
        Ok(())
    }
}

impl<T: Read> Read for Decoder<T> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self.source.read(buf) {
            Ok(n) => {
                fn advance_cursor(cur: (u32, u32), b: &u8) -> (u32, u32) {
                    let (l, ch) = cur;
                    match *b {
                        b'\n' => (l + 1, 1),
                        _ => (l, ch + 1),
                    }
                }
                let res: (u32, u32) = buf.iter().fold((self.line, self.ch), advance_cursor);
                self.line = res.0;
                self.ch = res.1;
                Ok(n)
            }
            res => res,
        }
    }
}

#[cfg(test)]
mod decoder_tests {
    use super::*;

    #[test]
    fn get_head_valid() {
        let buf = "begin 644 name of file.txt\n";
        let mut d = Decoder::from_bytes(buf.as_bytes());
        let (mode, fname) = d.head_ref().unwrap();
        assert_eq!(*mode, 0o644);
        assert_eq!(fname, "name of file.txt");
    }

    fn get_head_invalid<T: AsRef<[u8]>>(buf: T, err: Error) {
        let mut d = Decoder::from_bytes(buf.as_ref());
        let actual_err = d.head_ref().unwrap_err();
        assert_eq!(actual_err, err);
    }

    #[test]
    fn get_head_no_begin() {
        get_head_invalid("bein 644 name of file.txt\n",
                         Error::Syntax(SyntaxError {
                             kind: SyntaxErrorKind::ExpectedBegin,
                             line: 1,
                             ch: 1,
                         }))
    }

    #[test]
    fn get_head_begin_eof() {
        get_head_invalid("begi", Error::UnexpectedEof)
    }

    #[test]
    fn get_head_invalid_mode() {
        get_head_invalid("begin 644e name of file.txt\n",
                         Error::Syntax(SyntaxError {
                             kind: SyntaxErrorKind::UnexectedModeChar('e'),
                             line: 1,
                             ch: 10,
                         }))
    }

    #[test]
    fn get_head_mode_eof() {
        get_head_invalid("begin 64", Error::UnexpectedEof)
    }

    #[test]
    fn get_head_name_eof() {
        get_head_invalid("begin 644 name of", Error::UnexpectedEof)
    }

    #[test]
    fn decode_cat() {
        let src = "begin 644 cat.txt\n#0V%T\n`\nend\n";

        let mode = 0o644;
        let fname = "cat.txt";
        let body = "Cat";

        let mut d = Decoder::from_bytes(src.as_bytes());
        let mut dst = Vec::new();
        d.decode_to(&mut dst).unwrap();

        assert_eq!(dst, body.as_bytes());

        let (dec_mode, dec_fname) = d.head_ref().unwrap();
        assert_eq!(dec_fname, fname);
        assert_eq!(*dec_mode, mode);
    }

    fn decode_invalid<T: AsRef<[u8]>>(src: T, error: Error) {
        use std::io::sink;

        let mut d = Decoder::from_bytes(src.as_ref());
        let actual_err = d.decode_to(sink()).unwrap_err();
        assert_eq!(actual_err, error);
    }

    #[test]
    fn decode_empty_line() {
        let src = "begin 644 cat.txt\n#0V%T\n\n`\nend\n";
        decode_invalid(&src,
                       Error::Syntax(SyntaxError {
                           kind: SyntaxErrorKind::EmptyLine,
                           line: 3,
                           ch: 1,
                       }))
    }

    #[test]
    fn decode_short_line() {
        let src = "begin 644 cat.txt\n#0V\n`\nend\n";
        decode_invalid(&src,
                       Error::Syntax(SyntaxError {
                           kind: SyntaxErrorKind::ShortLine,
                           line: 2,
                           ch: 1,
                       }))
    }

    #[test]
    fn decode_long_line() {
        let src = "begin 644 cat.txt\n#0V%TXX\n`\nend\n";
        decode_invalid(&src,
                       Error::Syntax(SyntaxError {
                           kind: SyntaxErrorKind::LongLine,
                           line: 2,
                           ch: 1,
                       }))
    }

    #[test]
    fn decode_wrong_end() {
        let src = "begin 644 cat.txt\n#0V%T\n`\nwrong";
        decode_invalid(&src,
                       Error::Syntax(SyntaxError {
                           kind: SyntaxErrorKind::ExpectedEnd,
                           line: 4,
                           ch: 1,
                       }))
    }

    #[test]
    fn decode_text() {
        let src = r#"begin 444 file.txt
M22!F965L('9E<GD@<W1R;VYG;'D@86)O=70@>6]U(&1O:6YG(&1U='DN(%=O
M=6QD('EO=2!G:79E(&UE(&$@;&ET=&QE(&UO<F4@9&]C=6UE;G1A=&EO;B!A
M8F]U="!Y;W5R(')E861I;F<@:6X@1G)E;F-H/R!)(&%M(&=L860@>6]U(&%R
M92!H87!P>2#B@)0@8G5T($D@;F5V97(@8F5L:65V92!M=6-H(&EN(&AA<'!I
M;F5S<RX@22!N979E<B!B96QI979E(&EN(&UI<V5R>2!E:71H97(N(%1H;W-E
M(&%R92!T:&EN9W,@>6]U('-E92!O;B!T:&4@<W1A9V4@;W(@=&AE('-C<F5E
M;B!O<B!T:&4@<')I;G1E9"!P86=E<RP@=&AE>2!N979E<B!R96%L;'D@:&%P
4<&5N('1O('EO=2!I;B!L:69E+@H 
`
end
"#;

        let mode = 0o444;
        let fname = "file.txt";
        let body = "I feel very strongly about you doing duty. Would you give me a \
                    little more documentation about your reading in French? I am glad \
                    you are happy — but I never believe much in happiness. I never \
                    believe in misery either. Those are things you see on the stage \
                    or the screen or the printed pages, they never really happen to \
                    you in life.\n";

        let mut d = Decoder::from_bytes(src.as_bytes());
        let mut dst = Vec::new();
        d.decode_to(&mut dst).unwrap();

        assert_eq!(dst, body.as_bytes());

        let (dec_mode, dec_fname) = d.head_ref().unwrap();
        assert_eq!(dec_fname, fname);
        assert_eq!(*dec_mode, mode);
    }
}
