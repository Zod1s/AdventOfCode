use std::fs;

fn main() {
    let file = fs::read_to_string("input.txt").expect("Error: couldn't read file");
    let mut depths = Vec::new();

    for line in file.lines() {
        depths.push(line.parse::<usize>().expect("Error: non integer found"));
    }

    // let mut counter = 0;

    // for i in 1..depths.len() {
    //     if depths[i] > depths[i - 1] {
    //         counter += 1;
    //     }
    // }

    // println!("{}", counter);

    let mut windows = Vec::new();

    for i in 0..depths.len() - 2 {
        windows.push(depths[i] + depths[i + 1] + depths[i + 2]);
    }

    let mut counter = 0;

    for i in 1..windows.len() {
        if windows[i] > windows[i - 1] {
            counter += 1;
        }
    }

    println!("{}", counter);
}
