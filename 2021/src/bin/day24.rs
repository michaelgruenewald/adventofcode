use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Register {
    W,
    X,
    Y,
    Z,
}

impl From<&str> for Register {
    fn from(s: &str) -> Self {
        match s {
            "w" => Self::W,
            "x" => Self::X,
            "y" => Self::Y,
            "z" => Self::Z,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum RegImm {
    Immediate(isize),
    Register(Register),
}

impl From<&str> for RegImm {
    fn from(s: &str) -> Self {
        if let Ok(v) = s.parse() {
            Self::Immediate(v)
        } else {
            Self::Register(Register::from(s))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Instruction {
    Inp(Register),
    Add(Register, RegImm),
    Mul(Register, RegImm),
    Div(Register, RegImm),
    Mod(Register, RegImm),
    Eql(Register, RegImm),
}

fn parse(input: &str) -> Vec<Instruction> {
    input
        .split_terminator('\n')
        .map(
            |line| match line.split_whitespace().collect::<Vec<_>>()[..] {
                ["inp", dst] => Instruction::Inp(dst.into()),
                ["add", dst, src] => Instruction::Add(dst.into(), src.into()),
                ["mul", dst, src] => Instruction::Mul(dst.into(), src.into()),
                ["div", dst, src] => Instruction::Div(dst.into(), src.into()),
                ["mod", dst, src] => Instruction::Mod(dst.into(), src.into()),
                ["eql", dst, src] => Instruction::Eql(dst.into(), src.into()),
                _ => panic!("Illegal instruction: {}", line),
            },
        )
        .collect()
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Hash)]
struct Alu {
    registers: [isize; 4],
}

impl Alu {
    fn run(&mut self, instrs: &[Instruction], input: isize) {
        for instr in instrs {
            match instr {
                Instruction::Inp(reg) => self.write(*reg, input),
                Instruction::Add(reg, regimm) => {
                    self.write(*reg, self.read_reg(*reg) + self.read_regimm(*regimm))
                }
                Instruction::Mul(reg, regimm) => {
                    self.write(*reg, self.read_reg(*reg) * self.read_regimm(*regimm))
                }
                Instruction::Div(reg, regimm) => {
                    self.write(*reg, self.read_reg(*reg) / self.read_regimm(*regimm))
                }
                Instruction::Mod(reg, regimm) => {
                    self.write(*reg, self.read_reg(*reg) % self.read_regimm(*regimm))
                }
                Instruction::Eql(reg, regimm) => self.write(
                    *reg,
                    (self.read_reg(*reg) == self.read_regimm(*regimm)) as _,
                ),
            };
        }
    }

    fn read_reg(&self, reg: Register) -> isize {
        self.registers[reg as usize]
    }

    fn read_regimm(&self, regimm: RegImm) -> isize {
        match regimm {
            RegImm::Immediate(v) => v,
            RegImm::Register(r) => self.read_reg(r),
        }
    }

    fn write(&mut self, reg: Register, value: isize) {
        self.registers[reg as usize] = value;
    }
}

pub fn both_parts(input: &str) -> (usize, usize) {
    let instructions = parse(input);

    let mut groups = vec![];
    for instr in instructions {
        if let Instruction::Inp(_) = instr {
            groups.push(vec![]);
        }
        groups.last_mut().unwrap().push(instr);
    }

    let mut states = HashMap::new();
    states.insert(Alu::default(), (0, 0));

    for group in &groups {
        let mut new_states = HashMap::new();
        for (alu, &(biggest, smallest)) in &states {
            for digit in 1..=9 {
                let mut new_alu = alu.clone();
                new_alu.run(group, digit);

                let new_biggest = biggest * 10 + digit as usize;
                let new_smallest = smallest * 10 + digit as usize;

                let e = new_states
                    .entry(new_alu)
                    .or_insert((new_biggest, new_smallest));

                *e = (e.0.max(new_biggest), e.1.min(new_smallest));
            }
        }
        states = new_states;
    }

    let (biggest, smallest): (Vec<_>, Vec<_>) = states
        .into_iter()
        .filter(|(alu, _num)| alu.read_reg(Register::Z) == 0)
        .map(|(_alu, v)| v)
        .unzip();

    (
        *biggest.iter().max().unwrap(),
        *smallest.iter().min().unwrap(),
    )
}

fn main() {
    let input = include_str!("input24.txt");
    let (part1, part2) = both_parts(input);
    println!("{} {}", part1, part2);
}
