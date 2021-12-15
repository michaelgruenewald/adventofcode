#[cfg(feature = "day1")]
mod day1;
#[cfg(feature = "day10")]
mod day10;
#[cfg(feature = "day11")]
mod day11;
#[cfg(feature = "day12")]
mod day12;
#[cfg(feature = "day13")]
mod day13;
#[cfg(feature = "day14")]
mod day14;
#[cfg(feature = "day15")]
mod day15;
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
#[cfg(feature = "day8")]
mod day8;
#[cfg(feature = "day9")]
mod day9;

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
    #[cfg(feature = "day8")]
    {
        let input8 = include_str!("input8.txt");
        println!("{} {}", day8::part1(input8), day8::part2(input8));
    }
    #[cfg(feature = "day9")]
    {
        let input9 = include_str!("input9.txt");
        println!("{} {}", day9::part1(input9), day9::part2(input9));
    }
    #[cfg(feature = "day10")]
    {
        let input10 = include_str!("input10.txt");
        println!("{} {}", day10::part1(input10), day10::part2(input10));
    }
    #[cfg(feature = "day11")]
    {
        let input11 = include_str!("input11.txt");
        println!("{} {}", day11::part1(input11), day11::part2(input11));
    }
    #[cfg(feature = "day12")]
    {
        let input12 = include_str!("input12.txt");
        println!("{} {}", day12::part1(input12), day12::part2(input12));
    }
    #[cfg(feature = "day13")]
    {
        let input13 = include_str!("input13.txt");
        println!("{}\n{}", day13::part1(input13), day13::part2(input13));
    }
    #[cfg(feature = "day14")]
    {
        let input14 = include_str!("input14.txt");
        println!("{}\n{}", day14::part1(input14), day14::part2(input14));
    }
    #[cfg(feature = "day15")]
    {
        let input15 = include_str!("input15.txt");
        println!("{} {}", day15::part1(input15), day15::part2(input15));
    }
}
