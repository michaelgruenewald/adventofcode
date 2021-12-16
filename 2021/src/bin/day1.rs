pub fn part1(input: &str) -> usize {
    let numbers = input
        .split_whitespace()
        .map(|x| x.parse::<usize>().expect("a number"));

    numbers
        .clone()
        .zip(numbers.skip(1))
        .filter(|(a, b)| a < b)
        .count()
}

pub fn part2(input: &str) -> usize {
    let numbers = input
        .split_whitespace()
        .map(|x| x.parse::<usize>().expect("a number"));

    let triples = numbers
        .clone()
        .zip(numbers.clone().skip(1))
        .zip(numbers.skip(2))
        .map(|((a, b), c)| a + b + c);

    triples
        .clone()
        .zip(triples.skip(1))
        .filter(|(a, b)| a < b)
        .count()
}

fn main() {
    let input = include_str!("input1.txt");
    println!("{} {}", part1(input), part2(input));
}

#[cfg(test)]
const EXAMPLE: &str = "199
200
208
210
200
207
240
269
260
263";

#[test]
fn test_part1() {
    assert_eq!(7, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(5, part2(EXAMPLE));
}
