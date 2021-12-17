use std::iter::successors;

#[derive(PartialEq)]
enum Outcome {
    Hit,
    TooHigh,
    Miss,
}
use Outcome::*;

fn run(input: &str) -> Vec<(isize, isize)> {
    let captures = regex::Regex::new(r#"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)"#)
        .unwrap()
        .captures(input)
        .unwrap();
    let xmin = captures[1].parse::<isize>().unwrap();
    let xmax = captures[2].parse::<isize>().unwrap();
    let ymin = captures[3].parse::<isize>().unwrap();
    let ymax = captures[4].parse::<isize>().unwrap();

    let mut good = vec![];
    for vx in 1..=xmax {
        for vy in ymin..=-ymin {
            let velocities = successors(Some((vx, vy)), |v| Some(((v.0 - 1).max(0), v.1 - 1)));
            let mut positions = velocities.scan((0, 0), |pos, v| {
                *pos = (pos.0 + v.0, pos.1 + v.1);
                Some(*pos)
            });
            let outcome = positions
                .find_map(|(x, y)| {
                    if xmin <= x && x <= xmax && ymin <= y && y <= ymax {
                        Some(Hit)
                    } else if x > xmax && y > ymax {
                        Some(TooHigh)
                    } else if y < ymin {
                        Some(Miss)
                    } else {
                        None
                    }
                })
                .unwrap();
            match outcome {
                Hit => good.push((vx, vy)),
                TooHigh => break,
                Miss => (),
            }
        }
    }
    good
}

pub fn part1(input: &str) -> isize {
    let max_vy = run(input).iter().map(|&(_vx, vy)| vy).max().unwrap();
    (1..=max_vy).sum::<isize>()
}

pub fn part2(input: &str) -> usize {
    run(input).len()
}

fn main() {
    let input = include_str!("input17.txt");
    println!("{} {}", part1(input), part2(input));
}

#[cfg(test)]
const EXAMPLE: &str = "target area: x=20..30, y=-10..-5";

#[test]
fn test_part1() {
    assert_eq!(45, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(112, part2(EXAMPLE));
}
