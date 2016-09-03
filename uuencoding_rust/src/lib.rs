//! UUE encoding and decoding library.
//!
//! # Usage Example:
//!
//! ```
//! extern crate uue;
//! use std::str::from_utf8;
//!
//! let filename = "cat.txt".to_string();
//! let mode = 0o644;
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

mod decode;
pub use decode::*;
