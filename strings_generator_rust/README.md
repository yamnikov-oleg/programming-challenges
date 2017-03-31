# String Generator

I made up this problem myself, just for fun.

## Description

It's simple: write a program that will generate a random string, which matches the user-provided regular expression.

For example, given `http://[a-z]+\.[a-z]{2,3}` the program should generate strings similar to the following:

```
http://iwckmo.whp
http://xpojgrsgdnkjltmb.ku
http://vjflqllaqr.gdn
```

# Implementation

This solution is written in _Rust Nightly 1.18_.

To run use:

```
cargo run -- "http://[a-z]+\.[a-z]{2,3}"
```
