//#[macro_use]
use simple_error::bail;
use std::error::Error;
use std::fs;
use std::collections::HashMap;
use regex::Regex;

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

pub fn run(config: Config) -> Result<i64, Box<dyn Error>> {
    let contents = fs::read_to_string(config.filename)?;
    let mut lines = contents.lines();
    let first: Directions;
    match lines.next() {
        Some(s) => first = parse(s.to_string()),
        _ => bail!("no first line found"),
    }
    let second: Directions;
    match lines.next() {
        Some(s) => second = parse(s.to_string()),
        _ => bail!("no first line found"),
    }
    // println!("{:?}", first);
    // println!("{:?}", second);

    let mut shortest_travel = i64::MAX;
    let mut closest: i64 = i64::MAX;
    let mut m = HashMap::new();
    let mut traveled = 0;
    for pos in first.journey_iter() {
        traveled+=1;
        m.insert(pos.clone(), traveled);
    }

    traveled = 0;
    for pos in second.journey_iter() {
        traveled+=1;
        // Part I
        if m.contains_key(&pos) {
            let dist = pos.0.abs() + pos.1.abs();
            if dist < closest {
                closest = dist;
            }
        }

        // Part II
        match m.get(&pos) {
            Some(x) => {
                let dist = x + traveled;
                if dist < shortest_travel {
                    shortest_travel = dist;
                }
            },
            _ => {},
        }
    }

    println!("closest: {:?}", closest);
    Ok(shortest_travel)
    // bail!("no match found");
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Direction {
    N(i64),
    E(i64),
    S(i64),
    W(i64),
}

#[derive(Debug, Clone)]
struct Directions(Vec<Direction>);

impl Directions {
    fn journey_iter(self) -> DirectionsIter {
        DirectionsIter{
            dirs: self.0.clone(),
            curr_index: 0,
            curr_loc: (0, 0),
            curr_dist: 0,
        }
    }
}

#[derive(Debug, Clone)]
struct DirectionsIter {
    dirs: Vec<Direction>,
    curr_index: usize,
    curr_loc: (i64, i64),
    curr_dist: i64,
}

impl Iterator for DirectionsIter {
    type Item = (i64, i64);

    fn next(&mut self) -> Option<(i64, i64)> {
        if self.curr_index >= self.dirs.len() {
            return None;
        }

        let curr_dir = &self.dirs[self.curr_index];
        let dir: (i64, i64);
        let amount: i64;
        match curr_dir {
            Direction::N(n) => {amount = *n; dir = (0, 1);},
            Direction::S(n) => {amount = *n; dir = (0, -1);},
            Direction::E(n) => {amount = *n; dir = (1, 0);},
            Direction::W(n) => {amount = *n; dir = (-1, 0);},
        }
        self.curr_loc = (self.curr_loc.0+dir.0, self.curr_loc.1+dir.1);
        self.curr_dist+=1;
        if self.curr_dist >= amount {
            // gone enough, move to next:
            self.curr_index+=1;
            self.curr_dist = 0;
        }
        Some(self.curr_loc)
    }
}

fn parse(s: String) -> Directions {
    let parts = s.split(',');
    // Could use lazy_static here
    let re = Regex::new(r"([UDLR])(\d+)").unwrap();

    // Currently we just throw away things that don't make sense:
    let dirs = parts.filter_map(|d| re.captures(d)).filter_map(|cap| {
        let dir = cap.get(1).map_or("", |m| m.as_str());
        let n: i64 = cap.get(2).map_or("", |m| m.as_str()).parse::<i64>().ok()?;
        match dir {
            "U" => Some(Direction::N(n)),
            "R" => Some(Direction::E(n)),
            "D" => Some(Direction::S(n)),
            "L" => Some(Direction::W(n)),
            _ => None,
        }
    }).collect();

    Directions(dirs)
}
