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

pub fn run(config: Config) -> Result<i64, Box<dyn Error>> {
    let contents = fs::read_to_string(config.filename)?;
    let parts = contents.split(',');
    // There has to be a better looking way than this? But any attempt to get into a one-liner
    // seems to fail:
    let base_code_res: Result<Vec<i64>, _> = parts.map(|p| p.parse::<i64>()).collect();
    let base_code: Vec<i64> = base_code_res?;

    // part I - give a 1:
    let out = run_code_with_start(&base_code, vec![2]);

    println!("out {:?}", out);
    Ok(out[0])
}

fn run_code_with_start(code: &Vec<i64>, inputs: Vec<i64>) -> Vec<i64> {
    let mut state = init_state(code.clone(), inputs);
    while state.status == Status::Running {
        state = step(state);
    }
    state.outputs
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

fn init_state(code: Vec<i64>, inputs: Vec<i64>) -> State {
    let mut map_code = HashMap::new();
    for i in 0..code.len() {
        map_code.insert(i, code[i]);
    }
    State {
        status: Status::Running,
        code: map_code,
        at: 0,
        relative_base: 0,
        inputs: inputs,
        outputs: Vec::new(),
    }
}

#[derive(Debug, Clone)]
struct State {
    status: Status,
    relative_base: i64,
    code: HashMap<usize, i64>,
    at: usize,
    inputs: Vec<i64>,
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
            // should really not just unwrap!
            *new_state
                .code
                .entry(new_state.get_address(s.at + 1, first_param))
                .or_insert(0) = new_state.inputs.pop().unwrap();
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
