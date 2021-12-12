use std::collections::HashMap;

fn run(input: &str, ex: bool) -> usize {
    let mut positions = HashMap::new();
    input
        .trim()
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .for_each(|pos| {
            positions.insert(pos, positions.get(&pos).unwrap_or(&0) + 1);
        });

    let left = *positions.keys().min().unwrap();
    let right = *positions.keys().max().unwrap();

    let mut costs = vec![0; right + 1];

    for (&pos, &crabs) in positions.iter() {
        for (i, p) in (left..=pos)
            .rev()
            .enumerate()
            .chain((pos..=right).enumerate())
        {
            costs[p] += crabs * if ex { (0..=i).sum::<usize>() } else { i };
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
