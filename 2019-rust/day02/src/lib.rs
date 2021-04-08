#[macro_use]
extern crate simple_error;
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
    let parts = contents.split(',');
    // There has to be a better looking way than this? But any attempt to get into a one-liner
    // seems to fail:
    let base_code_res: Result<Vec<usize>, _> = parts.map(|p| p.parse::<usize>()).collect();
    let base_code: Vec<usize> = base_code_res?;

    // part I
    // let mut starting_code = base_code.clone();
    // starting_code[1] = 12;
    // starting_code[2] = 2;
    //
    // let mut state = State{ status: Status::Running, code: starting_code, at: 0};
    // while state.status == Status::Running {
    //     state = step(state);
    // }
    let mut state: State;
    for noun in 0..100 {
        for verb in 0..100 {
            let mut starting_code = base_code.clone();
            starting_code[1] = noun;
            starting_code[2] = verb;
            state = State{ status: Status::Running, code: starting_code, at: 0};
            while state.status == Status::Running {
                state = step(state);
            }
            if state.code[0] == 19690720 {
                println!("{} {}", noun, verb);
                return Ok(100*noun + verb);
            }
        }
    }
    bail!("no match found");
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Status {
    Running,
    Completed,
    Errored
}

#[derive(Debug, Clone)]
struct State {
    status: Status,
    code: Vec<usize>,
    at: usize,
}

fn step(s: State) -> State {
    let mut new_state = s.clone();
    // TODO: proper array bound checking:
    match s.code[s.at] {
        99 => {
            new_state.status = Status::Completed;
        },
        1 => {
            // add
            new_state.code[s.code[s.at+3]] = s.code[s.code[s.at+1]] + s.code[s.code[s.at+2]];
            new_state.at += 4;
        },
        2 => {
            // mult
            new_state.code[s.code[s.at+3]] = s.code[s.code[s.at+1]] * s.code[s.code[s.at+2]];
            new_state.at += 4;
        },
        _ => new_state.status = Status::Errored,
    }
    new_state
}