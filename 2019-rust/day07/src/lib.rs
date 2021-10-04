use itertools::Itertools;
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

    // Part I
    // let it = (0..5).permutations(5);

    // let mut max_value = 0;
    // for comb in it {
    //     let val = run_phase_setting(&base_code, comb);
    //     if val > max_value {
    //         max_value = val;
    //     }
    // }

    // Ok(max_value)

    let it = (5..10).permutations(5);

    let mut max_value = 0;
    for comb in it {
        let val = run_phase_setting_part_two(&base_code, comb).unwrap();
        if val > max_value {
            max_value = val;
        }
    }

    Ok(max_value)
}

fn run_phase_setting(code: &Vec<i64>, phases: Vec<i64>) -> i64 {
    let mut prev_res = 0;
    for phase in phases {
        prev_res = run_code_with_start(code, vec![prev_res, phase]);
    }
    prev_res
}

fn run_code_with_start(code: &Vec<i64>, inputs: Vec<i64>) -> i64 {
    let mut state = State {
        status: Status::Running,
        code: code.clone(),
        at: 0,
        inputs: inputs,
        outputs: Vec::new(),
    };
    while state.status == Status::Running {
        state = step(state);
    }
    state.outputs[0]
}

fn run_phase_setting_part_two(code: &Vec<i64>, phases: Vec<i64>) -> Option<i64> {
    let n = phases.len();
    let mut amps: Vec<State> = Vec::new();
    // setting up
    for phase in phases {
        let s = State {
            status: Status::Running,
            code: code.clone(),
            at: 0,
            inputs: vec![phase],
            outputs: Vec::new(),
        };
        amps.push(s);
    }
    // Add the first starting var in:
    amps[0].inputs.insert(0, 0);
    // time to run:
    let mut loopback_value = 0;
    loop {
        for i in 0..n {
            while amps[i].status == Status::Running && amps[i].outputs.len() == 0 {
                amps[i] = step_borrom(&amps[i]);
            }
            while amps[i].outputs.len() > 0 {
                let val = amps[i].outputs.pop()?;
                if i == n - 1 {
                    loopback_value = val;
                }
                amps[(i + 1) % n].inputs.insert(0, val);
            }
        }
        if amps[amps.len() - 1].status == Status::Completed {
            break;
        }
    }
    Some(loopback_value)
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
    inputs: Vec<i64>,
    outputs: Vec<i64>,
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
    step_borrom(&s)
}

fn step_borrom(s: &State) -> State {
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
            // should really not just unwrap!
            new_state.code[s.code[s.at + 1] as usize] = new_state.inputs.pop().unwrap();
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
