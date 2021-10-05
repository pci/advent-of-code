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
    // There has to be a better looking way than this? But any attempt to get into a one-liner
    // seems to fail:
    let image_res: Result<Vec<usize>, _> = contents
        .chars()
        .map(|p| p.to_string().parse::<usize>())
        .collect();
    let image: Vec<usize> = image_res?;

    let w = 25;
    let h = 6;

    // Part I
    let img = process(image, w, h);
    let counts = img.layers.iter().map(count);
    let mut min_zeros = w * h;
    let mut corresponding_mult = 0;
    for c in counts {
        // unwraps are safe due to the ok_or
        let zeros = c.get(&0).ok_or(0).unwrap();
        let ones = c.get(&1).ok_or(0).unwrap();
        let twos = c.get(&2).ok_or(0).unwrap();
        if zeros < &min_zeros {
            min_zeros = *zeros;
            corresponding_mult = ones * twos;
        }
    }

    // part II
    let final_img = part_two(&img);
    for row in final_img.data {
        // just easier to print out to read out:
        println!("{:?}", row)
    }
    Ok(corresponding_mult)
}

fn process(img: Vec<usize>, w: usize, h: usize) -> Image {
    let mut layers = Vec::new();

    // assume it's a multiple:
    let n = img.len() / (w * h);

    for l in 0..n {
        let mut layer = Layer { data: Vec::new() };
        for j in 0..h {
            let mut row = Vec::new();
            for i in 0..w {
                row.push(img[l * h * w + j * w + i])
            }
            layer.data.push(row);
        }
        layers.push(layer);
    }

    Image {
        h: h,
        w: w,
        layers: layers,
    }
}

#[derive(Debug, Clone)]
struct Image {
    h: usize,
    w: usize,
    layers: Vec<Layer>,
}

#[derive(Debug, Clone)]
struct Layer {
    data: Vec<Vec<usize>>,
}

fn count(l: &Layer) -> HashMap<&usize, usize> {
    let mut counts = HashMap::new();
    for row in &l.data {
        for pixel in row {
            *counts.entry(pixel).or_insert(0) += 1
        }
    }
    counts
}

fn part_two(img: &Image) -> Layer {
    // 0 - black
    // 1 - white
    // 2 - transparent
    let mut l = Layer { data: Vec::new() };
    for j in 0..img.h {
        let mut row = Vec::new();
        for i in 0..img.w {
            for n in 0..(img.layers.len()) {
                let pixel = img.layers[n].data[j][i];
                if pixel == 0 {
                    row.push(0);
                    break;
                }
                if pixel == 1 {
                    row.push(1);
                    break;
                }
            }
        }
        l.data.push(row);
    }
    l
}
