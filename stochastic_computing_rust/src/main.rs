#[macro_use]
extern crate clap;
extern crate rand;

use clap::{App, Arg};
use rand::prelude::*;

#[derive(Debug)]
enum Op {
    Add,
    Sub,
    Mult,
}

fn parse(expr: &str) -> Result<(f64, Op, f64), String> {
    let (op, ix) = match (expr.find('+'), expr.find('-'), expr.find('*')) {
        (Some(ix), None, None) => (Op::Add, ix),
        (None, Some(ix), None) => (Op::Sub, ix),
        (None, None, Some(ix)) => (Op::Mult, ix),
        (_, _, _) => {
            return Err("Expression cannot contain more than one operator".to_owned());
        }
    };
    let n1 = expr[..ix].parse::<f64>().map_err(|e| format!("{}", e))?;
    let n2 = expr[ix + 1..].parse::<f64>().map_err(|e| format!("{}", e))?;
    Ok((n1, op, n2))
}

struct StochValue<R: Rng> {
    prob: f64,
    rng: R,
}

impl StochValue<ThreadRng> {
    fn new(prob: f64) -> Self {
        Self {
            prob,
            rng: thread_rng(),
        }
    }
}

impl<R: Rng> StochValue<R> {
    fn next(&mut self) -> bool {
        self.rng.gen::<f64>() < self.prob
    }
}

struct StochMachine {
    op: Op,
    scale: StochValue<ThreadRng>,
}

impl StochMachine {
    fn new(op: Op, scale: f64) -> Self {
        Self {
            op,
            scale: StochValue::new(scale),
        }
    }

    fn calc(&mut self, a: bool, b: bool) -> bool {
        match self.op {
            Op::Add => if self.scale.next() {
                b
            } else {
                a
            },
            Op::Sub => if self.scale.next() {
                !b
            } else {
                a
            },
            Op::Mult => a && b,
        }
    }

    fn calc_many<R1: Rng, R2: Rng>(
        &mut self,
        a: &mut StochValue<R1>,
        b: &mut StochValue<R2>,
        bits: usize,
    ) -> f64 {
        let mut trues = 0;
        for _ in 0..bits {
            if self.calc(a.next(), b.next()) {
                trues += 1;
            }
        }
        (trues as f64) / (bits as f64)
    }
}

fn main() {
    let matches = App::new("stoc")
        .arg(
            Arg::with_name("EXPR")
                .required(true)
                .help("The expression, e.g. 0.5+0.25 or 0.1*0.76"),
        )
        .arg(
            Arg::with_name("bits")
                .short("b")
                .long("bits")
                .default_value("100000")
                .help("Number of bits to run through the machine"),
        )
        .arg(
            Arg::with_name("scale")
                .short("s")
                .long("scale")
                .default_value("0.5")
                .help("Scaling factor used for addition and subtraction"),
        )
        .get_matches();

    let expression = matches.value_of("EXPR").unwrap();
    let (n1, op, n2) = match parse(expression) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let scale = value_t!(matches, "scale", f64).unwrap_or(0.5);
    let mut machine = StochMachine::new(op, scale);
    let bits = value_t!(matches, "bits", usize).unwrap_or(100000);
    let res = machine.calc_many(&mut StochValue::new(n1), &mut StochValue::new(n2), bits);

    println!("Result: {} ({} bits precision)", res, bits);
}
