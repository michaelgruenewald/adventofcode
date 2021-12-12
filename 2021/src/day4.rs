use std::collections::HashMap;

struct Input {
    numbers: Vec<usize>,
    boards: Vec<Vec<Vec<usize>>>,
}

fn parse_input(input: &str) -> Input {
    let mut sections = input.split_terminator("\n\n");
    let numbers = sections
        .next()
        .unwrap()
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect();

    let boards = sections
        .map(|section| {
            section
                .split_terminator('\n')
                .map(|line| {
                    line.split_whitespace()
                        .map(|s| s.parse::<usize>().unwrap())
                        .collect()
                })
                .collect()
        })
        .collect();

    Input { numbers, boards }
}

struct State {
    positions: HashMap<(usize, usize), usize>,
    values: HashMap<usize, (usize, usize)>,
    won: bool,
}

#[derive(PartialEq)]
enum Which {
    First,
    Last,
}

fn run(input: &str, which: Which) -> usize {
    let data = parse_input(input);
    let mut state = data
        .boards
        .iter()
        .map(|m| {
            let positions = m
                .iter()
                .enumerate()
                .flat_map(|(row, r)| r.iter().enumerate().map(move |(col, n)| ((row, col), *n)))
                .collect::<HashMap<_, _>>();
            let values = positions.iter().map(|(pos, n)| (*n, *pos)).collect();
            State {
                positions,
                values,
                won: false,
            }
        })
        .collect::<Vec<_>>();

    let mut boards_left = state.len();
    for n in data.numbers {
        for board in &mut state {
            if let Some(pos) = board.values.remove(&n) {
                board.positions.remove(&pos).unwrap();
            }
            if !board.won {
                for i in 0..5 {
                    if (0..5).all(|j| !board.positions.contains_key(&(j, i)))
                        || (0..5).all(|j| !board.positions.contains_key(&(i, j)))
                    {
                        board.won = true;
                        boards_left -= 1;
                        break;
                    }
                }
                if which == Which::First && board.won || which == Which::Last && boards_left == 0 {
                    return n * board.values.keys().sum::<usize>();
                }
            }
        }
    }

    panic!()
}

pub fn part1(input: &str) -> usize {
    run(input, Which::First)
}

pub fn part2(input: &str) -> usize {
    run(input, Which::Last)
}

#[cfg(test)]
const EXAMPLE: &str = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7";

#[test]
fn test_part1() {
    assert_eq!(4512, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(1924, part2(EXAMPLE));
}
