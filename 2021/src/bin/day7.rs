use std::collections::HashMap;

fn run(input: &str, ex: bool) -> usize {
    let mut positions = HashMap::<_, usize>::new();
    input
        .trim()
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .for_each(|pos| *positions.entry(pos).or_default() += 1);

    let _left = *positions.keys().min().unwrap();
    let right = *positions.keys().max().unwrap();

    let mut costs = vec![0; right + 1];

    for (&pos, &crabs) in positions.iter() {
        for (p, cost) in costs.iter_mut().enumerate() {
            let distance = pos.abs_diff(p);
            *cost += crabs * if ex { (0..=distance).sum() } else { distance };
        }
    }

    *costs.iter().min().unwrap()
}

pub fn part1(input: &str) -> usize {
    run(input, false)
}

pub fn part2(input: &str) -> usize {
    run(input, true)
}

fn main() {
    let input = include_str!("input7.txt");
    println!("{} {}", part1(input), part2(input));
}

#[cfg(test)]
const EXAMPLE: &str = "16,1,2,0,4,2,7,1,2,14";

#[test]
fn test_part1() {
    assert_eq!(37, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    // This one is weird because the example mentions 206 for position two,
    // but position two isn't the cheapest anymore.
    assert_eq!(168, part2(EXAMPLE));
}
