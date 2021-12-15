use regex::Regex;
use std::collections::HashSet;

#[derive(Debug)]
enum Axis {
    X,
    Y,
}

#[derive(Debug)]
struct Instruction {
    axis: Axis,
    position: isize,
}

impl Instruction {
    fn execute(&self, dots: &HashSet<(isize, isize)>) -> HashSet<(isize, isize)> {
        dots.iter()
            .map(|&(x, y)| match self.axis {
                Axis::X if x > self.position => (2 * self.position - x, y),
                Axis::Y if y > self.position => (x, 2 * self.position - y),
                _ => (x, y),
            })
            .collect()
    }
}

impl From<&str> for Instruction {
    fn from(line: &str) -> Self {
        let captures = Regex::new(r"^fold along ([xy])=(\d+)$")
            .unwrap()
            .captures(line)
            .unwrap();
        Self {
            axis: match &captures[1] {
                "x" => Axis::X,
                "y" => Axis::Y,
                _ => unreachable!(),
            },
            position: captures[2].parse().unwrap(),
        }
    }
}

#[derive(Debug)]
struct Input {
    dots: HashSet<(isize, isize)>,
    instructions: Vec<Instruction>,
}

impl From<&str> for Input {
    fn from(input: &str) -> Self {
        let (dots_s, instructions_s) = input.split_once("\n\n").unwrap();

        let dots = dots_s
            .split_terminator('\n')
            .map(|line| {
                let (x, y) = line.split_once(',').unwrap();
                (x.parse().unwrap(), y.parse().unwrap())
            })
            .collect();
        let instructions = instructions_s
            .split_terminator('\n')
            .map(Instruction::from)
            .collect();

        Self { dots, instructions }
    }
}

pub fn part1(input: &str) -> usize {
    let data = Input::from(input);
    data.instructions[0].execute(&data.dots).len()
}

pub fn part2(input: &str) -> String {
    let data = Input::from(input);
    let final_dots = data
        .instructions
        .iter()
        .fold(data.dots, |dots, instruction| instruction.execute(&dots));

    (0..=final_dots.iter().map(|&(_x, y)| y).max().unwrap())
        .map(|y| {
            "\n".to_owned()
                + (0..=final_dots.iter().map(|&(x, _y)| x).max().unwrap())
                    .map(|x| {
                        if final_dots.contains(&(x, y)) {
                            '█'
                        } else {
                            '░'
                        }
                    })
                    .collect::<String>()
                    .as_str()
        })
        .collect::<String>()
}

#[cfg(test)]
const EXAMPLE: &str = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
";

#[test]
fn test_part1() {
    assert_eq!(17, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(
        "
█████
█░░░█
█░░░█
█░░░█
█████",
        part2(EXAMPLE)
    );
}
