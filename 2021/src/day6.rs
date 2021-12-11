fn run(input: &str, days: usize) -> usize {
    let mut generations = vec![0; 9];
    input
        .trim()
        .split(",")
        .map(|s| s.parse::<usize>().unwrap())
        .for_each(|fish| generations[fish] += 1);

    for day in 0..days {
        generations[(day + 7) % 9] += generations[day % 9];
    }

    generations.iter().sum()
}

pub fn part1(input: &str) -> usize {
    run(input, 80)
}

pub fn part2(input: &str) -> usize {
    run(input, 256)
}

#[cfg(test)]
const EXAMPLE: &str = "3,4,3,1,2";

#[test]
fn test_part1() {
    assert_eq!(5934, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(26984457539, part2(EXAMPLE));
}
