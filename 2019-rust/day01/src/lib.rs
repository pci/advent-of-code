
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

pub fn run(config: Config) -> Result<i32, Box<dyn Error>> {
    let contents = fs::read_to_string(config.filename)?;
    let lines = contents.lines();

    let mut sum = 0;
    for l in lines {
        let x = l.parse::<i32>()?;
        sum += calc_modules_fuel_need(x)
    }

    Ok(sum)
}

fn calc_modules_fuel_need(mass: i32) -> i32 {
    let mut fuel = mass/3 - 2;

    // part II
    let mut delta = fuel;
    while delta > 0 {
        delta = delta/3 - 2;
        if delta < 0 {
            delta = 0;
        }
        fuel += delta;
    }

    fuel
}