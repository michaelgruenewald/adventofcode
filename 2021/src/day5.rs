use regex::Regex;
use std::cmp::max;
use std::collections::HashMap;

struct Line {
    x1: isize,
    y1: isize,
    x2: isize,
    y2: isize,
}

fn parse(input: &str) -> Vec<Line> {
    let line_re = Regex::new(r"(\d+),(\d+) -> (\d+),(\d+)").unwrap();
    input
        .split_terminator("\n")
        .map(|line| {
            let captures = line_re.captures(line).unwrap();
            Line {
                x1: captures[1].parse::<isize>().unwrap(),
                y1: captures[2].parse::<isize>().unwrap(),
                x2: captures[3].parse::<isize>().unwrap(),
                y2: captures[4].parse::<isize>().unwrap(),
            }
        })
        .collect()
}

fn run(lines: &[Line], with_diagonals: bool) -> usize {
    let mut pixels = HashMap::<(isize, isize), usize>::new();

    for line in lines {
        let xdist = isize::abs(line.x2 - line.x1);
        let xdir = isize::signum(line.x2 - line.x1);
        let ydist = isize::abs(line.y2 - line.y1);
        let ydir = isize::signum(line.y2 - line.y1);

        assert!(xdist == 0 || ydist == 0 || xdist == ydist);

        if with_diagonals || xdist == 0 || ydist == 0 {
            for d in 0..=max(xdist, ydist) {
                let pos = (line.x1 + d * xdir, line.y1 + d * ydir);
                pixels.insert(pos, pixels.get(&pos).unwrap_or(&0) + 1);
            }
        }
    }
    pixels.values().filter(|v| **v >= 2).count()
}

pub fn part1(input: &str) -> usize {
    let lines = parse(input);
    run(&lines, false)
}

pub fn part2(input: &str) -> usize {
    let lines = parse(input);
    run(&lines, true)
}

#[cfg(test)]
const EXAMPLE: &str = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
";

#[test]
fn test_part1() {
    assert_eq!(5, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(12, part2(EXAMPLE));
}
