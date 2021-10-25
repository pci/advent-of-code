use std::collections::HashMap;
use std::error::Error;
use std::io::{stdout, Write};
use std::{fs, thread, time};

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
    let stdout = stdout();
    let mut stdout = stdout.lock();
    let contents = fs::read_to_string(config.filename)?;
    let parts = contents.split(',');
    // There has to be a better looking way than this? But any attempt to get into a one-liner
    // seems to fail:
    let base_code_res: Result<Vec<i64>, _> = parts.map(|p| p.parse::<i64>()).collect();
    let base_code: Vec<i64> = base_code_res?;

    // part II:
    // board is Map<(x,y), color>
    let mut board: HashMap<(i64, i64), i64> = HashMap::new();
    let mut state = init_state(base_code);
    // free play
    state.code.insert(0, 2);
    let mut score = 0;

    // super-advanced-auto-playing-AI
    // Note - since it auto-plays, we don't
    // _need_ to print it out to the terminal
    // ...but where would the fun be in that?!?
    let mut last_seen_ball_pos;
    let mut curr_ball_pos = (-1, -1);
    let mut curr_paddle = (0, 0);

    print!("{}{}", termion::clear::All, termion::cursor::Goto(1, 1));

    loop {
        if let (x_state, Some(x)) = run_to_next_output(&state) {
            let (y_state, y_opt) = run_to_next_output(&x_state);
            let (type_state, type_opt) = run_to_next_output(&y_state);
            state = type_state;
            // assume the next two values will work:
            let y = y_opt.unwrap();
            let t = type_opt.unwrap();
            if x < 0 {
                print!("{}Score: {:?}", termion::cursor::Goto(1, 1), t);
                score = t;
            } else {
                // one line for score
                print!("{}", termion::cursor::Goto((x + 1) as u16, (y + 2) as u16));
                match t {
                    // empty
                    0 => print!(" "),
                    // wall
                    1 => print!("X"),
                    // block
                    2 => print!("b"),
                    // paddle
                    3 => {
                        curr_paddle = (x, y);
                        print!("-");
                    }
                    // ball
                    4 => {
                        last_seen_ball_pos = curr_ball_pos;
                        curr_ball_pos = (x, y);
                        // special starting case:
                        if last_seen_ball_pos.0 == -1 {
                            last_seen_ball_pos = (curr_ball_pos.0 - 1, curr_ball_pos.1 - 1);
                        }
                        // y-1 to just go underneath it
                        let dy = (curr_paddle.1 - 1) - curr_ball_pos.1;
                        let dx = curr_ball_pos.0 - last_seen_ball_pos.0;
                        let target = dx * dy + curr_ball_pos.0;
                        print!("{}", termion::cursor::Goto((x + 1) as u16, (y + 2) as u16));
                        if curr_ball_pos.1 - last_seen_ball_pos.1 < 0 {
                            // ball travelling upwards, follow:
                            if dx < 0 && curr_paddle.0 >= curr_ball_pos.0 {
                                state.input = -1;
                            } else if dx > 0 && curr_paddle.0 <= curr_ball_pos.0 {
                                state.input = 1;
                            } else {
                                state.input = 0;
                            }
                        } else {
                            // otherwise go to where the ball will be
                            if target < curr_paddle.0 {
                                state.input = -1;
                            } else if target > curr_paddle.0 {
                                state.input = 1;
                            } else {
                                state.input = 0;
                            }
                        }

                        print!("0");
                        stdout.flush().unwrap();
                        thread::sleep(time::Duration::from_millis(50));
                    }
                    _ => {}
                }
                board.insert((x, y), t);
            }
        } else {
            break;
        }
    }

    // let mut block_count = 0;
    // for (_, v) in board {
    //     // blocks - 2
    //     if v == 2 {
    //         block_count += 1;
    //     }
    // }
    Ok(score)
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Status {
    Running,
    Completed,
    Errored,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Mode {
    Position,
    Immediate,
    Relative,
}

fn init_state(code: Vec<i64>) -> State {
    let mut map_code = HashMap::new();
    for i in 0..code.len() {
        map_code.insert(i, code[i]);
    }
    State {
        status: Status::Running,
        code: map_code,
        at: 0,
        relative_base: 0,
        input: 0,
        outputs: Vec::new(),
    }
}

#[derive(Debug, Clone)]
struct State {
    status: Status,
    relative_base: i64,
    code: HashMap<usize, i64>,
    at: usize,
    input: i64,
    outputs: Vec<i64>,
}

impl State {
    fn get(&self, at: usize, mode: Mode) -> i64 {
        let index = self.get_address(at, mode);
        *self.code.get(&index).unwrap_or(&0)
    }
    fn get_address(&self, at: usize, mode: Mode) -> usize {
        match mode {
            Mode::Immediate => at,
            // Assume good casting (shouldn't do this in production code!)
            Mode::Position => *self.code.get(&at).unwrap_or(&0) as usize,
            Mode::Relative => (*self.code.get(&at).unwrap_or(&0) + self.relative_base) as usize,
        }
    }
}

fn run_to_next_output(starting_state: &State) -> (State, Option<i64>) {
    let mut s = starting_state.clone();
    while s.status == Status::Running {
        s = step(s);
        if s.outputs.len() > 0 {
            let val = s.outputs.pop();
            return (s, val);
        }
    }
    (s, None)
}

fn step(s: State) -> State {
    step_borrom(&s)
}

fn step_borrom(s: &State) -> State {
    let mut new_state = s.clone();

    // tens and units are the command
    let full_command = s.code[&s.at];
    let opcode = full_command % 100;
    let first_param: Mode;
    if (full_command / 100) % 10 == 0 {
        first_param = Mode::Position;
    } else if (full_command / 100) % 10 == 1 {
        first_param = Mode::Immediate;
    } else {
        first_param = Mode::Relative;
    }
    let second_param: Mode;
    if (full_command / 1000) % 10 == 0 {
        second_param = Mode::Position;
    } else if (full_command / 1000) % 10 == 1 {
        second_param = Mode::Immediate;
    } else {
        second_param = Mode::Relative;
    }
    let third_param: Mode;
    if (full_command / 10000) % 10 == 0 {
        third_param = Mode::Position;
    } else if (full_command / 10000) % 10 == 1 {
        third_param = Mode::Immediate;
    } else {
        third_param = Mode::Relative;
    }

    match opcode {
        99 => {
            new_state.status = Status::Completed;
        }
        1 => {
            // add
            *new_state
                .code
                .entry(new_state.get_address(s.at + 3, third_param))
                .or_insert(0) = s.get(s.at + 1, first_param) + s.get(s.at + 2, second_param);
            new_state.at += 4;
        }
        2 => {
            // mult
            *new_state
                .code
                .entry(new_state.get_address(s.at + 3, third_param))
                .or_insert(0) = s.get(s.at + 1, first_param) * s.get(s.at + 2, second_param);
            new_state.at += 4;
        }
        3 => {
            *new_state
                .code
                .entry(new_state.get_address(s.at + 1, first_param))
                .or_insert(0) = new_state.input;
            new_state.at += 2;
        }
        4 => {
            new_state.outputs.push(s.get(s.at + 1, first_param));
            new_state.at += 2;
        }
        5 => {
            // jump-if-true
            if s.get(s.at + 1, first_param) != 0 {
                new_state.at = s.get(s.at + 2, second_param) as usize;
            } else {
                new_state.at += 3;
            }
        }
        6 => {
            // jump-if-false
            if s.get(s.at + 1, first_param) == 0 {
                new_state.at = s.get(s.at + 2, second_param) as usize;
            } else {
                new_state.at += 3;
            }
        }
        7 => {
            // less-than
            if s.get(s.at + 1, first_param) < s.get(s.at + 2, second_param) {
                *new_state
                    .code
                    .entry(new_state.get_address(s.at + 3, third_param))
                    .or_insert(0) = 1;
            } else {
                *new_state
                    .code
                    .entry(new_state.get_address(s.at + 3, third_param))
                    .or_insert(0) = 0;
            }
            new_state.at += 4;
        }
        8 => {
            // equals
            if s.get(s.at + 1, first_param) == s.get(s.at + 2, second_param) {
                *new_state
                    .code
                    .entry(new_state.get_address(s.at + 3, third_param))
                    .or_insert(0) = 1;
            } else {
                *new_state
                    .code
                    .entry(new_state.get_address(s.at + 3, third_param))
                    .or_insert(0) = 0;
            }
            new_state.at += 4;
        }
        9 => {
            new_state.relative_base += s.get(s.at + 1, first_param);
            new_state.at += 2;
        }
        _ => new_state.status = Status::Errored,
    }
    new_state
}

#[allow(dead_code)]
fn print_board(b: &HashMap<(i64, i64), i64>, score: i64) {
    let mut right = 0;
    let mut bottom = 0;
    for (k, _) in b.clone() {
        if k.0 > right {
            right = k.0;
        }
        if k.1 > bottom {
            bottom = k.1;
        }
    }
    for y in 0..=bottom {
        for x in 0..=right {
            match b.get(&(x, y)).unwrap_or(&0) {
                // empty
                0 => print!(" "),
                // wall
                1 => print!("X"),
                // block
                2 => print!("b"),
                // paddle
                3 => print!("-"),
                // ball
                4 => print!("0"),
                _ => {}
            }
        }
        println!();
    }
    println!("Score: {:?}", score);
}
