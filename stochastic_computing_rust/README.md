# Create a Simple Stochastic Computing Machine

[Source](https://www.reddit.com/r/dailyprogrammer/comments/83754b/20180309_challenge_353_hard_create_a_simple/) (/r/dailyprogrammer)

## Description

[Stochastic computing](https://en.wikipedia.org/wiki/Stochastic_computing), first introduced by the noted scientist John von Neumann in 1953, is a collection of techniques that represent continuous values (for example probabilities between 0 and 1) using streams of random bits. The bits in the stream represent the probabilities. Complex computations can then be computed over these stream by applying simple bit-wise operations.

For example, given two probabilities p and q, using 8 bits, to represent the probabilities 0.5 and 0.25:

```
10011010
01010000
```

To calculate p x q we apply the logical AND over the bitstream:

```
00010000
```

Yielding 1/8, or 12.5%, the correct value. For an 8-bit stream, this is the smallest unit of precision we can calculate. To increase precision we must increase the size of the bitstream.

This approach has a few benefits in a noisy environment, most importantly a tolerance for loss (for example in transmission) and better fidelity over various bit lengths. However, precision comparable to a standard binary computer requires a significant amount of data - 500 bits in a stochastic computer to get to 32-bit precision in a binary computer.

Your challenge today is to build a basic stochastic computer for probabilistic inputs, supporting the four major arithmetic operations:

* Addition
* Subtraction
* Multiplication
* Division

Be sure to measure the precision and fidelity of your machine, I encourage you to explore the tradeoffs between space and accuracy.


## Implementation

Solved in Rust 1.26. Run with:

```
$ cargo run 0.1+0.2
$ cargo run 0.1-0.2
$ cargo run 0.1*0.2
$ cargo run 0.1+0.2 --bits 1000
$ cargo run 0.1+0.2 --scale 0.4
```

I've made _scaled_ addition in a way described in [this paper](https://pdfs.semanticscholar.org/9d54/e1af9eccf7cc4b30dca30537c8b44bbf896c.pdf). Input probabilities have range [0, 1], but their arithmetic sum has range [0, 2], which is not a valid probability. But if we define the sum as `s*a + (1-s)*b` by introducing the scaling factor `s`, we overcome this obstucle.

Scaled subtraction has been derived by the same logic as the scaled addition. The arithmetic subtraction of two probabilities `Pa - Pb` has a range of [-1, 1], but if we add 1 to it (`Pa - Pb + 1 = Pa + (1 - Pb)`), we get sum of `a` and `!b`, which can be scaled as well.

I haven't implemented division, but I believe it's achievable by using _JK flip-flop_, used in [the solution by /u/gabyjunior](https://www.reddit.com/r/dailyprogrammer/comments/83754b/20180309_challenge_353_hard_create_a_simple/dvh5n87/).
