use std::fs;

fn is_nice_round1(line: &str) -> bool {
    let do_not_want: [&str; 4] = ["ab", "cd", "pq", "xy"];
    let mut unwanted_substr = false;
    let mut has_consecutive = false;
    let mut vowels = 0;
    for i in 0..line.len() - 1 {
        let substr: &str = &line[i..i + 2];
        if do_not_want.iter().position(|&x| x == substr).is_some() {
            unwanted_substr = true;
            break;
        }

        if "aeiou".contains(substr.chars().nth(0).unwrap()) {
            vowels += 1;
        }

        if substr.chars().nth(0).unwrap() == substr.chars().nth(1).unwrap() {
            has_consecutive = true;
        }
    }

    if "aeiou".contains(line.chars().nth(line.len() - 1).unwrap()) {
        vowels += 1;
    }

    if unwanted_substr {
        return false;
    }
    return vowels >= 3 && has_consecutive;
}

fn main() {
    let binding = fs::read_to_string("5-input").expect("wget 5-input please.");
    let contents: Vec<&str> = binding.split("\n").collect();
    let nice_lines_1 = contents.iter().filter(|&x| is_nice_round1(x)).count();
    println!("Round 1: {nice_lines_1}");
}

// Local Variables:
// compile-command: "rustc puzzle-5.rs && ./puzzle-5"
// End:
