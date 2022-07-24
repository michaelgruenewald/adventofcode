use std::fmt::Debug;
use std::iter::Peekable;
use std::ops::Add;

#[derive(Clone)]
enum Node {
    Value(u32),
    Pair(Box<Node>, Box<Node>),
}

use Node::*;

impl Node {
    fn left_value_mut(&mut self) -> &mut u32 {
        match self {
            Pair(l, _) => l.left_value_mut(),
            Value(ref mut v) => v,
        }
    }

    fn right_value_mut(&mut self) -> &mut u32 {
        match self {
            Pair(_, r) => r.right_value_mut(),
            Value(ref mut v) => v,
        }
    }

    fn recurse_split(&mut self) -> bool {
        match self {
            Pair(left, right) => left.recurse_split() || right.recurse_split(),
            Value(v) => {
                if *v >= 10 {
                    *self = Pair(Value(*v / 2).into(), Value(*v - *v / 2).into());
                    true
                } else {
                    false
                }
            }
        }
    }

    fn recurse_explode(&mut self, level: usize) -> Option<(u32, u32)> {
        match self {
            Pair(left, right) => {
                if let (&Value(lv), &Value(rv), true) = (&**left, &**right, level >= 4) {
                    *self = Value(0);
                    Some((lv, rv))
                } else if let Some((ll, lr)) = left.recurse_explode(level + 1) {
                    *right.left_value_mut() += lr;
                    Some((ll, 0))
                } else if let Some((rl, rr)) = right.recurse_explode(level + 1) {
                    *left.right_value_mut() += rl;
                    Some((0, rr))
                } else {
                    None
                }
            }
            Value(_) => None,
        }
    }

    fn reduce_step(&mut self) -> bool {
        self.recurse_explode(0).is_some() || self.recurse_split()
    }

    fn reduce(mut self) -> Self {
        while self.reduce_step() {}
        self
    }

    fn magnitude(&self) -> u32 {
        match self {
            Value(v) => *v,
            Pair(l, r) => 3 * l.magnitude() + 2 * r.magnitude(),
        }
    }
}

impl Add for Node {
    type Output = Self;
    fn add(self, other: Self) -> Self::Output {
        Pair(self.into(), other.into()).reduce()
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Value(x) => write!(f, "{}", x),
            Pair(left, right) => write!(f, "[{:?},{:?}]", *left, *right),
        }
    }
}

fn parse<T: Iterator<Item = char>>(chars: &mut Peekable<T>) -> Node {
    match chars.peek() {
        Some('[') => {
            assert_eq!(Some('['), chars.next());
            let left = parse(chars);
            assert_eq!(Some(','), chars.next());
            let right = parse(chars);
            assert_eq!(Some(']'), chars.next());

            Pair(left.into(), right.into())
        }
        Some(c) if c.is_ascii_digit() => {
            let mut v = 0;
            while let Some(c) = chars.next_if(|c| c.is_ascii_digit()) {
                v = v * 10 + c.to_digit(10).unwrap();
            }

            Value(v)
        }
        _ => panic!(),
    }
}

pub fn part1(input: &str) -> u32 {
    input
        .split_terminator('\n')
        .map(|line| parse(&mut line.chars().peekable()))
        .reduce(|a, e| a + e)
        .unwrap()
        .magnitude()
}

pub fn part2(input: &str) -> u32 {
    let numbers = input
        .split_terminator('\n')
        .map(|line| parse(&mut line.chars().peekable()))
        .collect::<Vec<_>>();

    numbers
        .iter()
        .flat_map(|n1| numbers.iter().map(move |n2| (n1, n2)))
        .map(|(n1, n2)| (n1.clone() + n2.clone()).magnitude())
        .max()
        .unwrap()
}

fn main() {
    let input = include_str!("input18.txt");
    println!("{} {}", part1(input), part2(input));
}

#[cfg(test)]
const EXAMPLE: &str = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
";

#[test]
fn test_reduce_step() {
    for (before, after) in [
        ("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
        ("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
        ("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
        (
            "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]",
            "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
        ),
        (
            "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
            "[[3,[2,[8,0]]],[9,[5,[7,0]]]]",
        ),
    ] {
        let mut root = parse(&mut before.chars().peekable());
        root.reduce_step();
        assert_eq!(after, format!("{:?}", root));
    }
}

#[test]
fn test_reduce() {
    let root = parse(&mut "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]".chars().peekable());
    assert_eq!(
        "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]",
        format!("{:?}", root.reduce())
    );
}

#[test]
fn test_part1() {
    assert_eq!(4140, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(3993, part2(EXAMPLE));
}
