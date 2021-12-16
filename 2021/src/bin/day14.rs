use std::collections::HashMap;

fn run(input: &str, steps: usize) -> usize {
    let (template, rules_s) = input.split_once("\n\n").unwrap();
    let rules = rules_s
        .split_terminator('\n')
        .map(|line| {
            let (l, r) = line.split_once(" -> ").unwrap();
            (
                (l.chars().next().unwrap(), l.chars().nth(1).unwrap()),
                r.chars().next().unwrap(),
            )
        })
        .collect::<HashMap<_, _>>();

    let mut pairs: HashMap<_, usize> = HashMap::new();
    for pair in template.chars().zip(template.chars().skip(1)) {
        *pairs.entry(pair).or_default() += 1
    }

    for _step in 1..=steps {
        let mut new_pairs = HashMap::new();
        for ((l, r), count) in pairs {
            if let Some(&c) = rules.get(&(l, r)) {
                *new_pairs.entry((l, c)).or_default() += count;
                *new_pairs.entry((c, r)).or_default() += count;
            } else {
                // looks like this isn't really needed, but who cares?
                *new_pairs.entry((l, r)).or_default() += count;
            }
        }
        pairs = new_pairs;
    }

    // initialize with the very first letter and then only count the right characters of each pair
    let mut letters: HashMap<_, usize> = HashMap::from([(template.chars().next().unwrap(), 1)]);
    for ((_l, r), count) in pairs {
        *letters.entry(r).or_default() += count;
    }

    letters.values().max().unwrap() - letters.values().min().unwrap()
}

pub fn part1(input: &str) -> usize {
    run(input, 10)
}

pub fn part2(input: &str) -> usize {
    run(input, 40)
}

fn main() {
    let input = include_str!("input14.txt");
    println!("{} {}", part1(input), part2(input));
}

#[cfg(test)]
const EXAMPLE: &str = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C";

#[test]
fn test_part1() {
    assert_eq!(1588, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(2188189693529, part2(EXAMPLE));
}
