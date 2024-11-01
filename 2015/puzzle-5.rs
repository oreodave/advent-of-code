use std::fs;

const VOWELS_STR: &str = "aeiou";
const DO_NOT_WANT: [&str; 4] = ["ab", "cd", "pq", "xy"];

fn is_nice_round1(line: &str) -> bool {
    let mut unwanted_substr = false;
    let mut has_consecutive = false;
    let mut vowels = 0;
    for i in 0..line.len() - 1 {
        let slide: &str = &line[i..i + 2];
        if DO_NOT_WANT.iter().position(|&x| x == slide).is_some() {
            unwanted_substr = true;
            break;
        }
        let first = slide.chars().nth(0).unwrap();
        let second = slide.chars().nth(1).unwrap();
        if VOWELS_STR.contains(first) {
            vowels += 1;
        }
        if first == second {
            has_consecutive = true;
        }
    }

    if VOWELS_STR.contains(line.chars().nth(line.len() - 1).unwrap()) {
        vowels += 1;
    }

    if unwanted_substr {
        return false;
    }
    return vowels >= 3 && has_consecutive;
}

fn is_nice_round2(line: &str) -> bool {
    /*
    Justifying the O(|line|^2) runtime is easy: |line| is constant, such that at
    worst we're doing roughly O(256) iterations.  Not too bad.

    If |line| could be really big I'd look into optimising the rest, but why do
    that?
     */
    let mut has_pair = false;

    // Unfortunate O(|line|^2) runtime
    for i in 0..line.len() - 1 {
        let slide = &line[i..i + 2];
        let rest = &line[i + 2..];
        if rest.contains(slide) {
            has_pair = true;
            break;
        }
    }

    // O(|line|) runtime
    let mut has_triple = false;
    for i in 0..line.len() - 2 {
        let slide = &line[i..i + 3];
        if slide.chars().nth(0) == slide.chars().nth(2) {
            has_triple = true;
            break;
        }
    }

    return has_pair && has_triple;
}

fn main() {
    let binding = fs::read_to_string("5-input").expect("wget 5-input please.");
    let contents: Vec<&str> = binding.split("\n").collect();
    let nice_lines_1 = contents.iter().filter(|&x| is_nice_round1(x)).count();
    let nice_lines_2 = contents.iter().filter(|&x| is_nice_round2(x)).count();
    println!("Round 1: {nice_lines_1}");
    println!("Round 2: {nice_lines_2}");
}

// Local Variables:
// compile-command: "rustc puzzle-5.rs && ./puzzle-5"
// End:
