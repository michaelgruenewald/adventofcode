use std::collections::HashMap;
use std::collections::HashSet;

fn run(input: &str, allow_one_small_twice: bool) -> usize {
    let mut graph = HashMap::new();
    for line in input.split_terminator('\n') {
        let (from, to) = line.split_once('-').unwrap();

        if !graph.contains_key(from) {
            graph.insert(from, HashSet::new());
        }
        graph.get_mut(from).unwrap().insert(to);
        if !graph.contains_key(to) {
            graph.insert(to, HashSet::new());
        }
        graph.get_mut(to).unwrap().insert(from);
    }

    let mut paths = vec![(vec!["start"], false)];
    let mut total = 0;
    while let Some((path, saw_one_small_twice)) = paths.pop() {
        for &next in &graph[path.last().unwrap()] {
            if next == "end" {
                total += 1;
                continue;
            }

            let is_small = next.chars().all(|ch| ch.is_lowercase());
            let seen_before = path.contains(&next);
            if next == "start"
                || is_small && (seen_before && (!allow_one_small_twice || saw_one_small_twice))
            {
                continue;
            }

            let mut new_path = path.clone();
            new_path.push(next);

            paths.push((new_path, saw_one_small_twice || is_small && seen_before));
        }
    }

    total
}

pub fn part1(input: &str) -> usize {
    run(input, false)
}

pub fn part2(input: &str) -> usize {
    run(input, true)
}

#[cfg(test)]
mod test {
    use crate::day12::*;

    const EXAMPLE1: &str = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end";
    const EXAMPLE2: &str =
        "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc";
    const EXAMPLE3:&str="fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW";

    #[test]
    fn test_part1() {
        assert_eq!(10, part1(EXAMPLE1));
        assert_eq!(19, part1(EXAMPLE2));
        assert_eq!(226, part1(EXAMPLE3));
    }

    #[test]
    fn test_part2() {
        assert_eq!(36, part2(EXAMPLE1));
        assert_eq!(103, part2(EXAMPLE2));
        assert_eq!(3509, part2(EXAMPLE3));
    }
}
