enum Health {
    Illegal(char),
    Incomplete(Vec<char>),
    Complete,
}

use Health::*;

fn process(input: &str) -> impl Iterator<Item = Health> + '_ {
    input.split_terminator('\n').map(|line| {
        let mut stack = vec![];
        let mut chars = line.chars();
        loop {
            match (chars.next(), stack.last()) {
                (Some(c @ ('(' | '[' | '{' | '<')), _) => {
                    stack.push(c);
                }
                (Some(')'), Some('('))
                | (Some(']'), Some('['))
                | (Some('}'), Some('{'))
                | (Some('>'), Some('<')) => {
                    stack.pop();
                }
                (Some(c @ (')' | ']' | '}' | '>')), _) => break Illegal(c),
                (None, None) => break Complete,
                (None, Some(_)) => break Incomplete(stack),
                _ => panic!(),
            }
        }
    })
}

pub fn part1(input: &str) -> usize {
    process(input)
        .filter_map(|h| match h {
            Illegal(')') => Some(3),
            Illegal(']') => Some(57),
            Illegal('}') => Some(1197),
            Illegal('>') => Some(25137),
            _ => None,
        })
        .sum()
}

pub fn part2(input: &str) -> usize {
    let mut scores = process(input)
        .filter_map(|h| match h {
            Incomplete(stack) => Some(stack.iter().rev().fold(0, |a, e| {
                a * 5
                    + match e {
                        '(' => 1,
                        '[' => 2,
                        '{' => 3,
                        '<' => 4,
                        _ => unreachable!(),
                    }
            })),
            _ => None,
        })
        .collect::<Vec<_>>();
    scores.sort_unstable();
    scores[scores.len() / 2]
}

fn main() {
    let input = include_str!("input10.txt");
    println!("{} {}", part1(input), part2(input));
}

#[cfg(test)]
const EXAMPLE: &str = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
";

#[test]
fn test_part1() {
    assert_eq!(26397, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(288957, part2(EXAMPLE));
}
