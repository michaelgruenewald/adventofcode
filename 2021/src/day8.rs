use std::collections::hash_map::RandomState;
use std::collections::HashSet;

fn parse(input: &str) -> Vec<(Vec<&str>, Vec<&str>)> {
    input
        .split_terminator('\n')
        .map(|line| {
            let mut it = line.split('|');
            let patterns_s = it.next().unwrap().trim();
            let output_s = it.next().unwrap().trim();

            (
                patterns_s.split_whitespace().collect(),
                output_s.split_whitespace().collect(),
            )
        })
        .collect::<Vec<(Vec<_>, Vec<_>)>>()
}

pub fn part1(input: &str) -> usize {
    let entries = parse(input);

    entries
        .iter()
        .flat_map(|(_patterns, outputs)| {
            outputs.iter().map(|output| match output.len() {
                /*1*/ 2 | /*7*/ 3 | /*4*/ 4 | /*8*/ 7 => 1,
                _ => 0,
            })
        })
        .sum()
}

pub fn part2(input: &str) -> usize {
    let entries = parse(input);

    entries
        .iter()
        .map(|(patterns, outputs)| {
            let mut digits = vec![None; 10];

            digits[1] = patterns.iter().find(|p| p.len() == 2);
            digits[4] = patterns.iter().find(|p| p.len() == 4);
            digits[7] = patterns.iter().find(|p| p.len() == 3);
            digits[8] = patterns.iter().find(|p| p.len() == 7);

            let p235 = patterns
                .iter()
                .filter(|p| p.len() == 5)
                .cloned()
                .collect::<Vec<_>>();
            let p069 = patterns
                .iter()
                .filter(|p| p.len() == 6)
                .cloned()
                .collect::<Vec<_>>();

            let charset = |s: &str| HashSet::<char, RandomState>::from_iter(s.chars());

            digits[9] = p069.iter().find(|&p| {
                charset(p).is_superset(&charset(digits[4].unwrap()))
                    && charset(p).is_superset(&charset(digits[7].unwrap()))
            });
            digits[0] = p069.iter().find(|&p| {
                p != digits[9].unwrap() && charset(p).is_superset(&charset(digits[7].unwrap()))
            });
            digits[6] = p069
                .iter()
                .find(|&p| p != digits[9].unwrap() && p != digits[0].unwrap());
            digits[5] = p235
                .iter()
                .find(|&p| charset(p).is_subset(&charset(digits[6].unwrap())));
            digits[3] = p235
                .iter()
                .find(|&p| charset(p).is_superset(&charset(digits[1].unwrap())));
            digits[2] = p235
                .iter()
                .find(|&p| p != digits[3].unwrap() && p != digits[5].unwrap());

            outputs.iter().fold(0, |a, output| {
                a * 10
                    + digits
                        .iter()
                        .position(|&x| charset(x.unwrap()) == charset(output))
                        .unwrap()
            })
        })
        .sum()
}

#[cfg(test)]
const EXAMPLE: &str = "\
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
";

#[test]
fn test_part1() {
    assert_eq!(26, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(61229, part2(EXAMPLE));
}
