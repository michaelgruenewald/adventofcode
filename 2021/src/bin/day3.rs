pub fn part1(input: &str) -> usize {
    let numbers = input.split_terminator('\n').collect::<Vec<_>>();
    let length = numbers.len();
    let width = numbers[0].len();

    let mut sums = vec![0usize; width];
    for n in numbers {
        for (i, c) in n.chars().enumerate() {
            sums[i] += c.to_digit(2).unwrap() as usize;
        }
    }

    let gamma = sums
        .iter()
        .fold(0, |a, sum| a * 2 + (if *sum > length / 2 { 1 } else { 0 }));
    let epsilon = gamma ^ ((2 << (width - 1)) - 1);

    gamma * epsilon
}

pub fn part2(input: &str) -> usize {
    let mut numbers = input.split_terminator('\n').collect::<Vec<_>>();
    numbers.sort_unstable();

    let mut s = &numbers[..];
    for pos in 0.. {
        let split = s.partition_point(|&x| x.chars().nth(pos).unwrap() == '0');
        s = if split > s.len() / 2 {
            &s[..split]
        } else {
            &s[split..]
        };
        if s.len() == 1 {
            break;
        }
    }
    let oxygen = usize::from_str_radix(s[0], 2).unwrap();

    let mut s = &numbers[..];
    for pos in 0.. {
        let split = s.partition_point(|&x| x.chars().nth(pos).unwrap() == '0');
        s = if split > s.len() / 2 {
            &s[split..]
        } else {
            &s[..split]
        };
        if s.len() == 1 {
            break;
        }
    }
    let co2 = usize::from_str_radix(s[0], 2).unwrap();

    oxygen * co2
}

fn main() {
    let input = include_str!("input3.txt");
    println!("{} {}", part1(input), part2(input));
}

#[cfg(test)]
const EXAMPLE: &str = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010";

#[test]
fn test_part1() {
    assert_eq!(198, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(230, part2(EXAMPLE));
}
