//! UUE encoding and decoding library.
//!
//! # Usage Example:
//!
//! ```
//! extern crate uue;
//! use std::str::from_utf8;
//!
//! let filename = "cat.txt".to_string();
//! let mode = uue::PermMode::from_parts(6, 4, 4);
//! let input = Vec::from("Cat");
//! let mut output = Vec::new();
//!
//! let enc = uue::Encoder::from_bytes(filename, mode, input);
//! enc.encode_to(&mut output);
//!
//! assert_eq!(from_utf8(&output).unwrap(), "begin 644 cat.txt\n#0V%T\n`\nend\n")
//! ```

mod encode;
pub use encode::*;

use std::fmt;

#[derive(Clone, Copy)]
pub struct PermMode(u32);

impl PermMode {
    pub fn new(mode: u32) -> PermMode {
        PermMode(mode)
    }
    pub fn from_parts(u: u8, g: u8, a: u8) -> PermMode {
        PermMode(((u as u32) << 6) + ((g as u32) << 3) + (a as u32))
    }
}

impl fmt::Display for PermMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let rest = self.0;
        let (rest, a) = (rest / 8, rest % 8);
        let (rest, g) = (rest / 8, rest % 8);
        let (_, u) = (rest / 8, rest % 8);
        try!(write!(f, "{}{}{}", u, g, a));
        Ok(())
    }
}

impl fmt::Debug for PermMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <PermMode as fmt::Display>::fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn permode_display() {
        let cases = &[(PermMode::new(0), "000"),
                      (PermMode::new((6 << 6) + (4 << 3) + 4), "644"),
                      (PermMode::new((1 << 9) + (4 << 6) + (4 << 3) + 4), "444")];
        for c in cases {
            assert_eq!(format!("{}", c.0), c.1);
        }
    }
}
