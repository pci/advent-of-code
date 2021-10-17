//#[macro_use]
use num::integer::gcd;
use std::collections::HashMap;
use std::collections::HashSet;
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
    let mut lines = contents.lines();
    let mut line = 0;
    let mut points = HashSet::new();
    loop {
        match lines.next() {
            Some(s) => {
                let chars = s.chars();
                let mut index = 0;
                for c in chars {
                    match c {
                        '#' => {
                            points.insert(Point { x: index, y: line });
                        }
                        _ => {}
                    }
                    index += 1;
                }
            }
            None => break,
        }
        line += 1;
    }
    let (loc, _) = part_i(&points);
    print!("loc {:?}\n", loc);
    let two_hundereth = part_ii(&points, loc);
    Ok(100 * two_hundereth.x + two_hundereth.y)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Point {
    x: i64,
    y: i64,
}
fn direction(from: &Point, to: &Point) -> (Point, i64) {
    let dx = to.x - from.x;
    let dy = to.y - from.y;
    let n = gcd(dx.abs(), dy.abs());
    if n == 0 {
        return (Point { x: 0, y: 0 }, 0);
    }
    (
        Point {
            x: dx / n,
            y: dy / n,
        },
        n,
    )
}

fn part_i(s: &HashSet<Point>) -> (Point, usize) {
    let mut pos = Point { x: 0, y: 0 };
    let mut max = 0;
    for p in s {
        let mut can_see = HashSet::new();
        for q in s {
            if p != q {
                let (dir, _) = direction(p, q);
                can_see.insert(dir);
            }
        }
        if can_see.len() > max {
            max = can_see.len();
            pos = p.clone();
        }
    }
    (pos, max)
}

fn part_ii(s: &HashSet<Point>, loc: Point) -> Point {
    // Going to do a Map<turn, all_in_that_turn>

    // Start with a _unordered_ Map<angle, Set<(Point, distance)>>
    let mut by_angle: HashMap<Point, Vec<(&Point, i64)>> = HashMap::new();
    for p in s {
        if p == &loc {
            continue;
        }
        let (dir, distance) = direction(&loc, p);
        by_angle
            .entry(dir)
            .or_insert(Vec::new())
            .push((p, distance));
    }

    // Now for each angle we can order by distance then add into the overall Map<turn, Vec<(dir, point)>>
    let mut by_turn: Vec<Vec<(f64, Point)>> = Vec::new();
    for (angle, list) in by_angle {
        let mut ordered = list.clone();
        ordered.sort_by(|(_, da), (_, db)| da.cmp(db));
        // This is not my best coding ever...
        // I'm full of cold though so going to do this
        // case by case
        // Also I forgot - decreasing y is NORTH in this!
        let mut rad_angle;
        if angle.y == 0 {
            if angle.x > 0 {
                rad_angle = std::f64::consts::FRAC_PI_2;
            } else {
                rad_angle = 3.0 * std::f64::consts::FRAC_PI_2;
            }
        } else {
            rad_angle = ((angle.x as f64) / (-angle.y as f64)).atan();
            if angle.y > 0 {
                rad_angle += std::f64::consts::PI;
            }
            if rad_angle < 0.0 {
                rad_angle += 2.0 * std::f64::consts::PI;
            }
        }
        for i in 0..ordered.len() {
            let (o, _) = ordered[i];
            if let Some(entry) = by_turn.get_mut(i) {
                entry.push((rad_angle, o.clone()));
            } else {
                // always going to be the next one:
                by_turn.push(vec![(rad_angle, o.clone())])
            }
        }
    }

    // which turn takes us over 200?
    let mut asteroids_boom_count = 0;
    for i in 0..by_turn.len() {
        let mut round = by_turn[i].clone();
        // Debug - order all:
        round.sort_by(|(angle_a, _), (angle_b, _)| angle_a.partial_cmp(angle_b).unwrap());
        asteroids_boom_count += round.len();
        if asteroids_boom_count >= 200 {
            // this is the one!
            // first undo:
            asteroids_boom_count -= round.len();
            // step into christmas
            let (_, p) = &round[200 - (asteroids_boom_count + 1)];
            return p.clone();
        }
    }

    Point { x: 0, y: 0 }
}
