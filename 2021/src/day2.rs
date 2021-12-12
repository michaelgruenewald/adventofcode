use std::ops::Add;

enum Instr {
    Forward(usize),
    Down(usize),
    Up(usize),
}

impl Instr {
    fn parse(input: &str) -> impl Iterator<Item = Self> + '_ {
        input.split_terminator('\n').map(|line| {
            let mut words = line.split_whitespace();
            let direction = words.next().expect("first term");
            let distance = words
                .next()
                .expect("second term")
                .parse::<usize>()
                .expect("a number");
            match direction {
                "forward" => Instr::Forward(distance),
                "down" => Instr::Down(distance),
                "up" => Instr::Up(distance),
                _ => unimplemented!(),
            }
        })
    }
}

fn solve<State: Default + Add<Instr, Output = State> + Into<usize>>(input: &str) -> usize {
    Instr::parse(input)
        .fold::<State, _>(Default::default(), |state, instr| state + instr)
        .into()
}

// part 1

pub fn part1(input: &str) -> usize {
    solve::<State1>(input)
}

#[derive(Default)]
struct State1 {
    horizontal: usize,
    depth: usize,
}

impl Add<Instr> for State1 {
    type Output = Self;

    fn add(self, other: Instr) -> Self::Output {
        match other {
            Instr::Forward(x) => State1 {
                horizontal: self.horizontal + x,
                ..self
            },
            Instr::Down(x) => State1 {
                depth: self.depth + x,
                ..self
            },
            Instr::Up(x) => State1 {
                depth: self.depth - x,
                ..self
            },
        }
    }
}

impl From<State1> for usize {
    fn from(state: State1) -> Self {
        state.depth * state.horizontal
    }
}

// part 2

pub fn part2(input: &str) -> usize {
    solve::<State2>(input)
}

#[derive(Default)]
struct State2 {
    horizontal: usize,
    depth: usize,
    aim: usize,
}

impl Add<Instr> for State2 {
    type Output = Self;

    fn add(self, other: Instr) -> Self::Output {
        match other {
            Instr::Forward(x) => State2 {
                horizontal: self.horizontal + x,
                depth: self.depth + self.aim * x,
                ..self
            },
            Instr::Down(x) => State2 {
                aim: self.aim + x,
                ..self
            },
            Instr::Up(x) => State2 {
                aim: self.aim - x,
                ..self
            },
        }
    }
}

impl From<State2> for usize {
    fn from(state: State2) -> Self {
        state.depth * state.horizontal
    }
}

#[cfg(test)]
const EXAMPLE: &str = "forward 5
down 5
forward 8
up 3
down 8
forward 2
";

#[test]
fn test_part1() {
    assert_eq!(150, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(900, part2(EXAMPLE));
}
