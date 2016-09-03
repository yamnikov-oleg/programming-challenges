use std::fs;
use std::io;
use std::io::prelude::*;
use std::os::unix::fs::PermissionsExt;

/// Encodes any byte stream with uuencoding.
#[derive(Debug)]
pub struct Encoder<T: Read> {
    /// File name, displayed in the output header.
    pub filename: String,
    /// Permissions mode, displayed in the output header;
    pub mode: u32,
    /// Input stream, which is being encoded.
    pub reader: T,
}

impl Encoder<io::Cursor<Vec<u8>>> {
    /// Constructs an `Encoder`, which reads data from the buffer.
    ///
    /// #Usage:
    ///
    /// ```
    /// use uue::Encoder;
    ///
    /// let buf = Vec::new();
    /// let enc = Encoder::from_bytes("vfile.txt".to_string(), 0o644, buf);
    /// ```
    pub fn from_bytes(name: String, mode: u32, buf: Vec<u8>) -> Self {
        Encoder {
            filename: name,
            mode: mode,
            reader: io::Cursor::new(buf),
        }
    }
}

impl Encoder<fs::File> {
    /// Constructs an `Encoder` which reads data from `file`, which's name is assumed to be `name`.
    ///
    /// Permissions mode is extracted from file's metadata.
    ///
    /// # Usage:
    ///
    /// ```
    /// use std::fs;
    /// # use std::io::Result;
    /// use uue::Encoder;
    ///
    /// # fn f () -> Result<()> {
    /// let filename = "input.txt".to_string();
    /// let file = try!(fs::File::open(&filename));
    /// let enc = Encoder::from_file(filename, file);
    /// # Ok(())
    /// # }
    /// ```
    pub fn from_file(name: String, file: fs::File) -> Self {
        Encoder {
            filename: name,
            mode: file.metadata().unwrap().permissions().mode(),
            reader: file,
        }
    }
}

impl<T: Read> Encoder<T> {
    /// Constructs an `Encoder` with given fields.
    pub fn new(name: String, mode: u32, read: T) -> Self {
        Encoder {
            filename: name,
            mode: mode,
            reader: read,
        }
    }

    /// Consumes the encoder to read, encode and write encoded data into `dst`.
    ///
    /// To encode the data into `Vec<u8>`, pass it as `&mut`:
    ///
    /// ```
    /// use uue::Encoder;
    ///
    /// let input = &[1u8, 2u8, 3u8][..];
    /// let mut output = Vec::<u8>::new();
    ///
    /// let enc = Encoder::new("file.txt".to_string(), 0o644, input);
    /// enc.encode_to(&mut output);
    /// ```
    pub fn encode_to<W: Write>(mut self, mut dst: W) -> io::Result<()> {
        try!(write!(dst, "begin {:o} {}\n", self.mode, self.filename));

        loop {
            let mut buf = [0u8; 45];
            let size = match try!(self.reader.read(&mut buf)) {
                0 => break,
                s => s,
            };

            try!(dst.write(&[(size + 32) as u8]));

            let padded_size = match size % 3 {
                2 => size + 1,
                1 => size + 2,
                _ => size,
            };
            let line = &buf[..padded_size];

            for chunk in line.chunks(3) {
                let mut split: [u8; 4] = split_3b_in_4(chunk);
                for i in 0..4 {
                    split[i] += 32;
                }
                try!(dst.write(&split));
            }
            try!(dst.write(b"\n"));
        }

        try!(dst.write(b"`\nend\n"));

        Ok(())
    }
}

fn split_3b_in_4(bytes: &[u8]) -> [u8; 4] {
    assert!(bytes.len() >= 3);

    let mut total = ((bytes[0] as u32) << 16) + ((bytes[1] as u32) << 8) + (bytes[2] as u32);
    let mut out_bytes = [0; 4];

    let mut split_next_6 = || {
        let old_total = total;
        total = total >> 6;
        (old_total & 0b111111) as u8
    };

    for i in (0..4).rev() {
        out_bytes[i] = split_next_6();
    }
    out_bytes
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str;

    #[test]
    fn encoding_cat() {
        let filename = "cat.txt".to_string();
        let mode = 0o644;
        let input = Vec::from("Cat");
        let mut output = Vec::new();

        let enc = Encoder::from_bytes(filename, mode, input);
        enc.encode_to(&mut output).unwrap();

        assert_eq!(str::from_utf8(&output).unwrap(),
                   "begin 644 cat.txt\n#0V%T\n`\nend\n")
    }

    #[test]
    fn encoding_text() {
        let filename = "file.txt".to_string();
        let mode = 0o444;
        let input = Vec::from("I feel very strongly about you doing duty. Would you give me a \
                               little more documentation about your reading in French? I am glad \
                               you are happy — but I never believe much in happiness. I never \
                               believe in misery either. Those are things you see on the stage \
                               or the screen or the printed pages, they never really happen to \
                               you in life.\n");
        let mut output = Vec::new();
        let aim = r#"begin 444 file.txt
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

        let enc = Encoder::from_bytes(filename, mode, input);
        enc.encode_to(&mut output).unwrap();

        assert_eq!(str::from_utf8(&output).unwrap(), aim);
    }
}
