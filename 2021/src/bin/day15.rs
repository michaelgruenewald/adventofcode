use std::collections::HashMap;

fn parse(input: &str) -> HashMap<(isize, isize), isize> {
    let mut map = HashMap::new();

    for (line, y) in input.split_terminator('\n').zip(0..) {
        for (ch, x) in line.trim().chars().zip(0..) {
            map.insert((x, y), ch.to_digit(10).unwrap() as _);
        }
    }

    map
}

fn run(input: &str, repeats: isize) -> isize {
    let map = parse(input);
    let width = map.keys().map(|&(x, _y)| x).max().unwrap() + 1;
    let height = map.keys().map(|&(_x, y)| y).max().unwrap() + 1;

    let mut costs = HashMap::new();
    let mut tentative_costs = HashMap::from([((0, 0), 0)]);

    while let Some((&(x, y), &cost)) = tentative_costs.iter().min_by_key(|(_pos, &cost)| cost) {
        costs.insert((x, y), tentative_costs.remove(&(x, y)).unwrap());

        for (dx, dy) in [(0, -1), (0, 1), (-1, 0), (1, 0)] {
            let adj_x = x + dx;
            let adj_y = y + dy;
            if adj_x < 0 || adj_x >= width * repeats || adj_y < 0 || adj_y >= height * repeats {
                continue;
            }
            if costs.contains_key(&(adj_x, adj_y)) {
                continue;
            }
            let adj_cost = cost
                + (map.get(&(adj_x % width, adj_y % height)).unwrap()
                    + adj_x / width
                    + adj_y / height
                    - 1)
                    % 9
                + 1;

            let e = tentative_costs.entry((adj_x, adj_y)).or_insert(adj_cost);
            *e = (*e).min(adj_cost);
        }
    }

    *costs
        .get(&(width * repeats - 1, height * repeats - 1))
        .unwrap()
}

pub fn part1(input: &str) -> isize {
    run(input, 1)
}

pub fn part2(input: &str) -> isize {
    run(input, 5)
}

fn main() {
    let input = include_str!("input15.txt");
    println!("{} {}", part1(input), part2(input));
}

#[cfg(test)]
const EXAMPLE: &str = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
";

#[test]
fn test_part1() {
    assert_eq!(40, part1(EXAMPLE));
}

#[test]
fn test_part2() {
    assert_eq!(315, part2(EXAMPLE));
}
