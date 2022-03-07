use regex::Regex;
use std::fs;

fn main() {
    let file = fs::read_to_string("input.txt").expect("Error: couldn't read file");
    let mut depth = 0;
    let mut pos = 0;
    let mut aim = 0;

    let forward = Regex::new(r"forward (\d+)").unwrap();
    let down = Regex::new(r"down (\d+)").unwrap();
    let up = Regex::new(r"up (\d+)").unwrap();

    // for line in file.lines() {
    //     if let Some(f) = forward.captures(line) {
    //         pos += f[1].parse::<usize>().expect("Error: no integer found");
    //     } else if let Some(d) = down.captures(line) {
    //         depth += d[1].parse::<usize>().expect("Error: no integer found");
    //     } else if let Some(u) = up.captures(line) {
    //         depth -= u[1].parse::<usize>().expect("Error: no integer found");
    //     }
    // }

    for line in file.lines() {
        if let Some(f) = forward.captures(line) {
            let fo = f[1].parse::<usize>().expect("Error: no integer found");
            pos += fo;
            depth += fo * aim;
        } else if let Some(d) = down.captures(line) {
            aim += d[1].parse::<usize>().expect("Error: no integer found");
        } else if let Some(u) = up.captures(line) {
            aim -= u[1].parse::<usize>().expect("Error: no integer found");
        }
    }


    println!("{}", depth * pos);
}
