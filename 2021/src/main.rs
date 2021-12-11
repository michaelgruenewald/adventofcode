#[cfg(feature = "day1")]
mod day1;
#[cfg(feature = "day2")]
mod day2;
#[cfg(feature = "day3")]
mod day3;
#[cfg(feature = "day4")]
mod day4;
#[cfg(feature = "day5")]
mod day5;
#[cfg(feature = "day6")]
mod day6;
#[cfg(feature = "day7")]
mod day7;

fn main() {
    #[cfg(feature = "day1")]
    {
        let input1 = include_str!("input1.txt");
        println!("{} {}", day1::part1(input1), day1::part2(input1));
    }
    #[cfg(feature = "day2")]
    {
        let input2 = include_str!("input2.txt");
        println!("{} {}", day2::part1(input2), day2::part2(input2));
    }
    #[cfg(feature = "day3")]
    {
        let input3 = include_str!("input3.txt");
        println!("{} {}", day3::part1(input3), day3::part2(input3));
    }
    #[cfg(feature = "day4")]
    {
        let input4 = include_str!("input4.txt");
        println!("{} {}", day4::part1(input4), day4::part2(input4));
    }
    #[cfg(feature = "day5")]
    {
        let input5 = include_str!("input5.txt");
        println!("{} {}", day5::part1(input5), day5::part2(input5));
    }
    #[cfg(feature = "day6")]
    {
        let input6 = include_str!("input6.txt");
        println!("{} {}", day6::part1(input6), day6::part2(input6));
    }
    #[cfg(feature = "day7")]
    {
        let input7 = include_str!("input7.txt");
        println!("{} {}", day7::part1(input7), day7::part2(input7));
    }
}
