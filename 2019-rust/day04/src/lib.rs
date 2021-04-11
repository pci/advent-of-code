use regex::Regex;
// use simple_error::bail;
use std::error::Error;
use std::fs;

pub struct Config {
    pub filename: String,
}
impl Config {
    pub fn new(args: &[String]) -> Result<Config, &str> {
        if args.len() < 2 {
            return Err("not enough arguments");
        }

        let filename = args[1].clone();

        Ok(Config { filename })
    }
}

pub fn run(config: Config) -> Result<usize, Box<dyn Error>> {
    let contents = fs::read_to_string(config.filename)?;
    let (min, max) = parse(contents);

    println!("{:?}-{:?}", min, max);
    println!(
        "Part I: {}",
        make_iter(min.clone(), max.clone())
            .filter(|n| n.has_double())
            .collect::<Vec<Input>>()
            .len()
    );

    Ok(make_iter(min, max)
        .filter(|n| n.has_isolated_double())
        .collect::<Vec<Input>>()
        .len())
    // bail!("no match found");
}

#[derive(Debug, Clone, PartialEq)]
struct Input([i64; 6]);

impl Input {
    // part I
    fn has_double(&self) -> bool {
        for i in 0..5 {
            if self.0[i] == self.0[i + 1] {
                return true;
            }
        }
        return false;
    }
    // part II
    fn has_isolated_double(&self) -> bool {
        for i in 0..5 {
            if self.0[i] != self.0[i + 1] {
                continue;
            }
            if i != 0 && self.0[i - 1] == self.0[i] {
                // repeats on the left:
                continue;
            }
            if i != 4 && self.0[i + 2] == self.0[i] {
                // repeats on the right:
                continue;
            }
            // only get here if you pass all checks:
            return true;
        }
        return false;
    }
}

struct InputIter {
    max: Input,
    curr: Input,
}

// This is massive overkill but playing with iterators is fun!
impl Iterator for InputIter {
    type Item = Input;

    fn next(&mut self) -> Option<Input> {
        let mut less_than = false;
        for i in 0..6 {
            if self.curr.0[i] < self.max.0[i] {
                less_than = true;
                break;
            }
        }
        if !less_than {
            return None;
        }
        let last = self.curr.clone();

        // add one
        let mut i = 5;
        loop {
            self.curr.0[i] += 1;
            if self.curr.0[i] < 10 {
                break;
            }
            self.curr.0[i] = 0;
            if i == 0 {
                // can't go anywhere
                return None;
            }
            i -= 1;
        }
        // find the next in-order:
        let mut inc = self.curr.0[0];
        self.curr.0[0] = inc;
        for i in 1..6 {
            if self.curr.0[i] < inc {
                self.curr.0[i] = inc;
            }
            inc = self.curr.0[i];
        }

        // larger than max
        Some(last)
    }
}

fn make_iter(min: Input, max: Input) -> InputIter {
    let mut curr: [i64; 6] = [0, 0, 0, 0, 0, 0];
    let mut inc = min.0[0];
    curr[0] = inc;
    for i in 1..6 {
        if min.0[i] < inc {
            curr[i] = inc;
        } else {
            curr[i] = min.0[i];
        }
        inc = curr[i];
    }

    InputIter {
        max: max,
        curr: Input(curr),
    }
}

fn parse(s: String) -> (Input, Input) {
    // Could use lazy_static here
    let re = Regex::new(r"(\d{6})-(\d{6})").unwrap();

    // error on error:
    let cap = re.captures(&s).unwrap();
    return (
        parse_input_string(cap.get(1).map_or("", |m| m.as_str())),
        parse_input_string(cap.get(2).map_or("", |m| m.as_str())),
    );
}

fn parse_input_string(s: &str) -> Input {
    let mut ret: [i64; 6] = [0, 0, 0, 0, 0, 0];
    for i in 0..6 {
        // There must be a better way, but couldn't figure
        // out how to get a fixed size array out of a Iterator
        // .take(n) didn't quite work.
        match s.chars().nth(i).unwrap() {
            '1' => ret[i] = 1,
            '2' => ret[i] = 2,
            '3' => ret[i] = 3,
            '4' => ret[i] = 4,
            '5' => ret[i] = 5,
            '6' => ret[i] = 6,
            '7' => ret[i] = 7,
            '8' => ret[i] = 8,
            '9' => ret[i] = 9,
            _ => ret[i] = 0,
        }
    }
    Input(ret)
}
