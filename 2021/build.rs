use std::error::Error;
use std::fs;
use std::io::Write;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
    let src_dir = Path::new("src");

    let days = (1..=25)
        .filter(|day| src_dir.join(format!("day{}.rs", day)).exists())
        .collect::<Vec<_>>();

    let mut file = fs::File::create(src_dir.join("gen.rs"))?;
    for day in &days {
        writeln!(&mut file, "mod day{};", day)?;
    }
    writeln!(&mut file, "fn main() {{")?;
    for day in &days {
        writeln!(
            &mut file,
            r#"let input{} = include_str!("input{}.txt");"#,
            day, day
        )?;
        for part in 1..=2 {
            writeln!(
                &mut file,
                r#"println!("{}.{} {{}}", day{}::part{}(input{}));"#,
                day, part, day, part, day
            )?;
        }
    }
    //; println!("{{}}.1 {{}}\n{{}}", day{}::part1(input{}), day{}::part2(input{}));"#,
    writeln!(&mut file, "}}")?;

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src");

    Ok(())
}
