mod day1;
mod day2;
mod day3;

fn main() {
    let input1 = include_str!("input1.txt");
    println!("{} {}", day1::part1(input1), day1::part2(input1));
    let input2 = include_str!("input2.txt");
    println!("{} {}", day2::part1(input2), day2::part2(input2));
    let input3 = include_str!("input3.txt");
    println!("{} {}", day3::part1(input3), day3::part2(input3));
}
