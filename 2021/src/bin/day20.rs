use std::collections::HashMap;

use Pixel::*;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Pixel {
    Dark = 0,
    Light = 1,
}

const ADJ: [(isize, isize); 9] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 0),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

fn parse(input: &str) -> (Vec<Pixel>, HashMap<(isize, isize), Pixel>) {
    let (algo_input, image_input) = input.split_once("\n\n").unwrap();
    let algo = algo_input
        .chars()
        .filter_map(|c| match c {
            '#' => Some(Light),
            '.' => Some(Dark),
            _ => None,
        })
        .collect();

    let mut image = HashMap::new();
    for (line, row) in image_input.split_terminator('\n').zip(0..) {
        for (c, col) in line.chars().zip(0..) {
            match c {
                '#' => image.insert((row, col), Light),
                '.' => image.insert((row, col), Dark),
                _ => panic!(),
            };
        }
    }

    (algo, image)
}

fn run(input: &str, steps: usize) -> usize {
    let (algo, mut image) = parse(input);

    let mut infinity = Dark;

    for _step in 0..steps {
        let rmin = *image.keys().map(|(r, _c)| r).min().unwrap() - 1;
        let rmax = *image.keys().map(|(r, _c)| r).max().unwrap() + 1;
        let cmin = *image.keys().map(|(_r, c)| c).min().unwrap() - 1;
        let cmax = *image.keys().map(|(_r, c)| c).max().unwrap() + 1;

        let mut new_image = HashMap::new();
        let new_infinity = algo[infinity as usize * 0b111111111];

        for row in rmin..=rmax {
            for col in cmin..=cmax {
                let idx = ADJ
                    .iter()
                    .map(|(r, c)| {
                        image.get(&(row + r, col + c)).cloned().unwrap_or(infinity) == Light
                    })
                    .fold(0, |a, e| a * 2 + (e as usize));

                if algo[idx] != new_infinity {
                    new_image.insert((row, col), algo[idx]);
                }
            }
        }

        image = new_image;
        infinity = new_infinity;
    }

    assert_eq!(infinity, Dark);
    image.values().filter(|&c| *c == Light).count()
}

pub fn part1(input: &str) -> usize {
    run(input, 2)
}

pub fn part2(input: &str) -> usize {
    run(input, 50)
}

fn main() {
    let input = include_str!("input20.txt");
    println!("{} {}", part1(input), part2(input));
}

#[cfg(test)]
const EXAMPLE: &str = "\
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
";

#[test]
fn test_part1() {
    assert_eq!(35, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(3351, part2(EXAMPLE));
}
