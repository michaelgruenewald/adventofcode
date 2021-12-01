mod day1;

fn main() {
    let input1 = include_str!("input1.txt");
    println!("{} {}", day1::part1(input1), day1::part2(input1));
}
