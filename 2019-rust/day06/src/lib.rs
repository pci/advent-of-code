use regex::Regex;
//#[macro_use]
use simple_error::bail;
use std::collections::HashMap;
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
    let orbits = parse(&contents);

    // Part 1:
    let com = String::from("COM");
    let mut orbit_count = 0;
    for key in orbits.keys() {
        let mut nxt_com = orbits.get(key);
        loop {
            orbit_count += 1;
            match nxt_com {
                None => {
                    bail!("oribit without value");
                }
                Some(v) => {
                    if *v == com {
                        break;
                    }
                    nxt_com = orbits.get(v);
                }
            }
        }
    }
    println!("Part I: {:?}", orbit_count);
    // Ok(orbit_count)

    // Part II:
    // Get from YOU to SAN
    let you_list = get_list_to_com(&orbits, &String::from("YOU"))?;
    let san_list = get_list_to_com(&orbits, &String::from("SAN"))?;

    // first the first occurrance in you_list in san_list:
    let mut you_moves = 0;
    for you_entry in you_list {
        let mut san_moves = 0;
        for san_entry in &san_list {
            if you_entry == *san_entry {
                return Ok(you_moves + san_moves);
            }
            san_moves += 1
        }
        you_moves += 1;
    }

    Ok(0)
    // bail!("no match found");
}

// Ok so I hit the borrow checker on this one
// to start with I tried to make a bi-directional graph
// with a Body knowing it's children and containing a
// reference to its parent. However it turns out:
// Graphs. Are. Hard. In. Rust.
// So I when simple. For more on graphs see:
// https://aminb.gitbooks.io/rust-for-c/content/graphs/
// http://smallcultfollowing.com/babysteps/blog/2015/04/06/modeling-graphs-in-rust-using-vector-indices/

// Returns a map of key-orbits-value
fn parse(s: &str) -> HashMap<String, String> {
    // Could use lazy_static here
    let re = Regex::new(r"([^\)\n]+)\)([^\)\n]+)").unwrap();

    // This map is what 'owns' the nodes:
    let mut ret: HashMap<String, String> = HashMap::new();
    for cap in re.captures_iter(s) {
        let center_name = String::from(&cap[1]);
        let satellite_name = String::from(&cap[2]);
        ret.entry(satellite_name.clone())
            .or_insert(center_name.clone());
    }
    ret
}

fn get_list_to_com(
    orbits: &HashMap<String, String>,
    start: &String,
) -> Result<Vec<String>, Box<dyn Error>> {
    let mut ret: Vec<String> = Vec::new();

    let mut nxt_com = orbits.get(start);
    let com = String::from("COM");
    loop {
        match nxt_com {
            None => {
                bail!("oribit without value");
            }
            Some(v) => {
                ret.push(v.clone());
                if *v == com {
                    break;
                }
                nxt_com = orbits.get(v);
            }
        }
    }
    Ok(ret)
}
