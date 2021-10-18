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
    let parts = contents.split(',');
    // There has to be a better looking way than this? But any attempt to get into a one-liner
    // seems to fail:
    let base_code_res: Result<Vec<i64>, _> = parts.map(|p| p.parse::<i64>()).collect();
    let base_code: Vec<i64> = base_code_res?;

    // part I:
    // board is Map<(x,y), color>
    let mut board: HashMap<(i64, i64), i64> = HashMap::new();
    let mut location = (0, 0);
    let mut state = init_state(base_code, vec![]);
    let mut dir = (0, 1);

    // part II - start on white:
    board.insert((0, 0), 1);

    loop {
        // 1. Get current colour of location
        let current_color = *board.get(&location).unwrap_or(&0);
        // 2. Feed to state and get next two outputs
        if let Some((new_state, color, turn)) =
            run_state_with_one_input_and_get_two_outputs(&state, current_color)
        {
            state = new_state;
            // 3. Color
            board.insert(location, color);
            // 4. Turn
            match turn {
                0 => {
                    // Turn left
                    dir = (-dir.1, dir.0);
                }
                _ => {
                    // Turn right
                    dir = (dir.1, -dir.0);
                }
            }
            // 5. Move
            location.0 += dir.0;
            location.1 += dir.1;
        } else {
            // program exited
            break;
        }
    }

    // right, the fun of part two - outputing!
    // first find top, left, right:
    let mut top = 0;
    let mut bottom = 0;
    let mut left = 0;
    let mut right = 0;
    // why do I have to clone this two loop over the Map twice?
    let tblr_board = board.clone();
    for (k, v) in tblr_board {
        if v != 1 {
            // only care about the white squares
            continue;
        }
        if k.0 < left {
            left = k.0;
        }
        if k.0 > right {
            right = k.0;
        }
        if k.1 < top {
            top = k.1;
        }
        if k.1 > bottom {
            bottom = k.1;
        }
    }

    // Apparently this is how to do a bottom..=top..-1:
    for y in (top..=bottom).rev() {
        for x in left..=right {
            match board.get(&(x, y)).unwrap_or(&0) {
                // 'cos 1's and 0's were too hard to read!
                1 => {
                    print!("#")
                }
                _ => {
                    print!(" ")
                }
            }
        }
        println!();
    }

    Ok(board.keys().len())
}

fn run_state_with_one_input_and_get_two_outputs(
    s: &State,
    input: i64,
) -> Option<(State, i64, i64)> {
    let mut state = s.clone();
    state.inputs.push(input);
    let (first_round_state, first_option) = run_to_next_output(state);
    let first_val = first_option?;
    let (second_round_state, second_option) = run_to_next_output(first_round_state);
    let second_val = second_option?;
    Some((second_round_state, first_val, second_val))
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

fn run_to_next_output(starting_state: State) -> (State, Option<i64>) {
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
