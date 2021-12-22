use regex::Regex;
use std::collections::HashMap;
use std::mem::swap;

fn parse(input: &str) -> [PlayerState; 2] {
    let pattern = Regex::new(r#"Player (\d+) starting position: (\d+)"#).unwrap();
    let mut captures = pattern.captures_iter(input);

    [
        PlayerState {
            position: captures.next().unwrap()[2].parse::<usize>().unwrap(),
            score: 0,
        },
        PlayerState {
            position: captures.next().unwrap()[2].parse::<usize>().unwrap(),
            score: 0,
        },
    ]
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct PlayerState {
    position: usize,
    score: usize,
}

pub fn part1(input: &str) -> usize {
    let [mut player, mut other_player] = parse(input);

    let mut die = (1..=100).cycle();

    for turns in 1.. {
        let move_by = die.next().unwrap() + die.next().unwrap() + die.next().unwrap();
        player.position = (player.position + move_by - 1) % 10 + 1;
        player.score += player.position;
        if player.score >= 1000 {
            return other_player.score * turns * 3;
        }
        swap(&mut player, &mut other_player);
    }
    unreachable!()
}

pub fn part2(input: &str) -> u64 {
    let [initial_player, initial_other_player] = parse(input);

    let mut universes = HashMap::from([((initial_player, initial_other_player), 1u64)]);
    let mut player_won = 0;
    let mut other_player_won = 0;

    while !universes.is_empty() {
        let mut new_universes = HashMap::new();

        for ((player, other_player), count) in universes {
            for (move_by, copies) in [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)] {
                let position = (player.position + move_by - 1) % 10 + 1;
                let score = player.score + position;

                if score >= 21 {
                    player_won += count * copies;
                } else {
                    *new_universes
                        .entry((other_player.clone(), PlayerState { position, score }))
                        .or_default() += count * copies;
                }
            }
        }

        universes = new_universes;
        swap(&mut player_won, &mut other_player_won);
    }

    player_won.max(other_player_won)
}

fn main() {
    let input = include_str!("input21.txt");
    println!("{} {}", part1(input), part2(input));
}

#[cfg(test)]
const EXAMPLE: &str = "\
Player 1 starting position: 4
Player 2 starting position: 8
";

#[test]
fn test_part1() {
    assert_eq!(739785, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(444356092776315, part2(EXAMPLE));
}
