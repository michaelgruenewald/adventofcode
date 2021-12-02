mod day1;
mod day2;

fn main() {
    let input1 = include_str!("input1.txt");
    println!("{} {}", day1::part1(input1), day1::part2(input1));
    let input2 = include_str!("input2.txt");
    println!("{} {}", day2::part1(input2), day2::part2(input2));
}
