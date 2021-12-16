use std::collections::HashMap;
use std::collections::HashSet;

const ADJ: [(isize, isize); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

fn parse(input: &str) -> HashMap<(isize, isize), usize> {
    let mut map = HashMap::new();

    for (row, line) in input.split_terminator('\n').enumerate() {
        for (col, ch) in line.trim().chars().enumerate() {
            map.insert(
                (row as isize, col as isize),
                ch.to_digit(10).unwrap() as usize,
            );
        }
    }

    map
}

fn step(map: &mut HashMap<(isize, isize), usize>) -> usize {
    for e in map.values_mut() {
        *e += 1;
    }

    let mut flashed = HashSet::new();

    let mut todo = Vec::from_iter(map.keys().cloned());
    while let Some(pos) = todo.pop() {
        if map[&pos] > 9 && !flashed.contains(&pos) {
            flashed.insert(pos);
            for (drow, dcol) in ADJ {
                let apos = (pos.0 + drow, pos.1 + dcol);
                if let Some(e) = map.get_mut(&apos) {
                    *e += 1;
                    todo.push(apos);
                }
            }
        }
    }

    for pos in &flashed {
        *map.get_mut(pos).unwrap() = 0;
    }

    flashed.len()
}

pub fn part1(input: &str) -> usize {
    let mut map = parse(input);

    (1..=100).map(|_| step(&mut map)).sum()
}

pub fn part2(input: &str) -> usize {
    let mut map = parse(input);

    (1..).find(|_| step(&mut map) == map.len()).unwrap()
}

fn main() {
    let input = include_str!("input11.txt");
    println!("{} {}", part1(input), part2(input));
}

#[cfg(test)]
const EXAMPLE: &str = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
";

#[test]
fn test_part1() {
    assert_eq!(1656, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(195, part2(EXAMPLE));
}
