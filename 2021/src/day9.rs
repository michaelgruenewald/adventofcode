use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::HashSet;

const ADJ: [(isize, isize); 4] = [(0, -1), (0, 1), (-1, 0), (1, 0)];

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

fn low_points(map: &HashMap<(isize, isize), usize>) -> Vec<(isize, isize)> {
    map.iter()
        .filter(|((row, col), &h)| {
            ADJ.iter()
                .filter_map(|(drow, dcol)| map.get(&((row + drow), (col + dcol))))
                .all(|&v| h < v)
        })
        .map(|(&pos, _)| pos)
        .collect()
}

pub fn part1(input: &str) -> usize {
    let map = parse(input);
    low_points(&map).iter().map(|pos| map[pos] + 1).sum()
}

pub fn part2(input: &str) -> usize {
    let map = parse(input);
    low_points(&map)
        .iter()
        .map(|&pos| {
            let mut positions = HashSet::from([pos]);

            loop {
                let new_positions = HashSet::from_iter(positions.iter().flat_map(|(row, col)| {
                    ADJ.iter()
                        .map(move |(drow, dcol)| ((row + drow), (col + dcol)))
                        .filter(|apos| !positions.contains(apos))
                        .filter(|apos| map.get(apos).map(|&h| h < 9).unwrap_or(false))
                }));
                if new_positions.is_empty() {
                    break;
                }
                positions = &positions | &new_positions;
            }
            positions.len()
        })
        .collect::<BinaryHeap<_>>()
        .iter()
        .take(3)
        .product()
}

#[cfg(test)]
const EXAMPLE: &str = "2199943210
3987894921
9856789892
8767896789
9899965678
";

#[test]
fn test_part1() {
    assert_eq!(15, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(1134, part2(EXAMPLE));
}
