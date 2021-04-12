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

    let mut state: State;
    state = State {
        status: Status::Running,
        code: base_code,
        at: 0,
        // looking at the input code, this is the second command
        // that's run - very clever way to branch the code as you add
        // more commands!
        input: 5,
    };
    while state.status == Status::Running {
        state = step(state);
    }
    Ok(state.code[0])
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
}

#[derive(Debug, Clone)]
struct State {
    status: Status,
    code: Vec<i64>,
    at: usize,
    input: i64,
}

impl State {
    fn get(&self, at: usize, mode: Mode) -> i64 {
        match mode {
            Mode::Immediate => self.code[at],
            // Assume good casting (shouldn't do this in production code!)
            Mode::Position => self.code[self.code[at] as usize],
        }
    }
}

fn step(s: State) -> State {
    let mut new_state = s.clone();

    // tens and units are the command
    let full_command = s.code[s.at];
    let opcode = full_command % 100;
    let first_param: Mode;
    if (full_command / 100) % 10 > 0 {
        first_param = Mode::Immediate;
    } else {
        first_param = Mode::Position;
    }
    let second_param: Mode;
    if (full_command / 1000) % 10 > 0 {
        second_param = Mode::Immediate;
    } else {
        second_param = Mode::Position;
    }
    // TODO: add third param mode - (nothing uses it... yet)

    match opcode {
        99 => {
            new_state.status = Status::Completed;
        }
        1 => {
            // add
            new_state.code[s.code[s.at + 3] as usize] =
                s.get(s.at + 1, first_param) + s.get(s.at + 2, second_param);
            new_state.at += 4;
        }
        2 => {
            // mult
            new_state.code[s.code[s.at + 3] as usize] =
                s.get(s.at + 1, first_param) * s.get(s.at + 2, second_param);
            new_state.at += 4;
        }
        3 => {
            new_state.code[s.code[s.at + 1] as usize] = s.input;
            new_state.at += 2;
        }
        4 => {
            // Output, currently just output... to the terminal:
            println!("output: {:?}", s.get(s.at + 1, first_param));
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
                new_state.code[s.code[s.at + 3] as usize] = 1;
            } else {
                new_state.code[s.code[s.at + 3] as usize] = 0;
            }
            new_state.at += 4;
        }
        8 => {
            // equals
            if s.get(s.at + 1, first_param) == s.get(s.at + 2, second_param) {
                new_state.code[s.code[s.at + 3] as usize] = 1;
            } else {
                new_state.code[s.code[s.at + 3] as usize] = 0;
            }
            new_state.at += 4;
        }
        _ => new_state.status = Status::Errored,
    }
    new_state
}
