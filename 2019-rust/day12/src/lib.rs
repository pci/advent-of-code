use num::integer::lcm;
use regex::Regex;
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

pub fn run(config: Config) -> Result<i64, Box<dyn Error>> {
    let contents = fs::read_to_string(config.filename)?;
    let moons = parse(&contents);

    // Part 1: (needs mut on moons)
    // for i in 1..=10 {
    //     step_v(&mut moons);
    //     step_x(&mut moons);
    //     println!("step {:?}: {:?}", i, moons);
    // }
    // for _ in 0..1000 {
    //     step_v(&mut moons);
    //     step_x(&mut moons);
    // }
    // let te = total_energy(&moons);
    // println!("Part I: {:?}", moons);

    // Ok(te)

    // Part 2:
    println!("Before: {:?}", moons);

    let mut xs = Vec::new();
    let mut ys = Vec::new();
    let mut zs = Vec::new();
    for m in moons {
        xs.push(m.x.0);
        ys.push(m.x.1);
        zs.push(m.x.2);
    }
    let p_xs = calc_period(xs);
    println!("x period: {:?}", p_xs);
    let p_ys = calc_period(ys);
    println!("y period: {:?}", p_ys);
    let p_zs = calc_period(zs);
    println!("z period: {:?}", p_zs);
    let total_period = lcm(lcm(p_xs, p_ys), p_zs);
    Ok(total_period)
}

// Returns a map of key-orbits-value
fn parse(s: &str) -> Vec<Moon> {
    // Could use lazy_static here
    // <x=-8, y=-10, z=0>
    let re = Regex::new(r"<x=([^,]+), y=([^,]+), z=([^,]+)>").unwrap();

    // This vec is what 'owns' the moons:
    let mut ret: Vec<Moon> = vec![];
    for cap in re.captures_iter(s) {
        let x = (&cap[1]).parse::<i64>().unwrap();
        let y = (&cap[2]).parse::<i64>().unwrap();
        let z = (&cap[3]).parse::<i64>().unwrap();
        ret.push(Moon {
            x: (x, y, z),
            v: (0, 0, 0),
        });
    }
    ret
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Moon {
    x: (i64, i64, i64),
    v: (i64, i64, i64),
}
impl Moon {
    fn energy(&self) -> i64 {
        (self.x.0.abs() + self.x.1.abs() + self.x.2.abs())
            * (self.v.0.abs() + self.v.1.abs() + self.v.2.abs())
    }
}

#[allow(dead_code)]
fn total_energy(moons: &Vec<Moon>) -> i64 {
    let mut sum = 0;
    for m in moons {
        sum += m.energy()
    }
    sum
}

#[allow(dead_code)]
fn step_v(moons: &mut Vec<Moon>) {
    for i in 0..moons.len() {
        for j in 0..moons.len() {
            if i == j {
                continue;
            }
            if moons[i].x.0 < moons[j].x.0 {
                moons[i].v.0 += 1;
            }
            if moons[i].x.0 > moons[j].x.0 {
                moons[i].v.0 -= 1;
            }
            if moons[i].x.1 < moons[j].x.1 {
                moons[i].v.1 += 1;
            }
            if moons[i].x.1 > moons[j].x.1 {
                moons[i].v.1 -= 1;
            }
            if moons[i].x.2 < moons[j].x.2 {
                moons[i].v.2 += 1;
            }
            if moons[i].x.2 > moons[j].x.2 {
                moons[i].v.2 -= 1;
            }
        }
    }
}

#[allow(dead_code)]
fn step_x(moons: &mut Vec<Moon>) {
    for mut m in moons {
        m.x.0 += m.v.0;
        m.x.1 += m.v.1;
        m.x.2 += m.v.2;
    }
}

fn calc_period(pos: Vec<i64>) -> i64 {
    // assume it all goes back to zero velocities:
    let mut p_v = vec![];
    for i in 0..pos.len() {
        p_v.push((pos[i], 0));
    }

    let mut loop_count = 0;
    loop {
        loop_count += 1;
        for i in 0..(p_v.len() - 1) {
            for j in (i + 1)..p_v.len() {
                if p_v[i].0 < p_v[j].0 {
                    p_v[i].1 += 1;
                    p_v[j].1 -= 1;
                }
                if p_v[i].0 > p_v[j].0 {
                    p_v[i].1 -= 1;
                    p_v[j].1 += 1;
                }
            }
        }
        let mut all_same = true;
        for i in 0..p_v.len() {
            p_v[i].0 += p_v[i].1;
            if p_v[i].0 != pos[i] || p_v[i].1 != 0 {
                all_same = false;
            }
        }
        if all_same {
            break;
        }
    }
    loop_count
}
