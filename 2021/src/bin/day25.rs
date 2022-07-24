use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Place {
    East,
    South,
}

fn parse(input: &str) -> HashMap<(isize, isize), Place> {
    let mut map = HashMap::new();

    for (row, line) in input.split_terminator('\n').enumerate() {
        for (col, c) in line.chars().enumerate() {
            match c {
                '.' => {}
                '>' => {
                    map.insert((row as _, col as _), Place::East);
                }
                'v' => {
                    map.insert((row as _, col as _), Place::South);
                }
                _ => panic!(),
            }
        }
    }

    map
}

pub fn part1(input: &str) -> usize {
    let mut map = parse(input);
    let rows = *map.keys().map(|(row, _col)| row).max().unwrap() + 1;
    let cols = *map.keys().map(|(_row, col)| col).max().unwrap() + 1;

    for step in 1.. {
        let mut new_map = HashMap::new();

        for (&pos, &value) in map.iter().filter(|&(_pos, &value)| value == Place::East) {
            let new_pos = (pos.0, (pos.1 + 1) % cols);
            new_map.insert(
                if !map.contains_key(&new_pos) {
                    new_pos
                } else {
                    pos
                },
                value,
            );
        }

        for (&pos, &value) in map.iter().filter(|&(_pos, &value)| value == Place::South) {
            let new_pos = ((pos.0 + 1) % rows, pos.1);
            new_map.insert(
                if !new_map.contains_key(&new_pos) && map.get(&new_pos) != Some(&Place::South) {
                    new_pos
                } else {
                    pos
                },
                value,
            );
        }

        if new_map == map {
            return step;
        }

        map = new_map;
    }
    unreachable!()
}

fn main() {
    let input = include_str!("input25.txt");
    println!("{}", part1(input));
}

#[cfg(test)]
const EXAMPLE: &str = "\
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
";

#[test]
fn test_part1() {
    assert_eq!(58, part1(EXAMPLE));
}
