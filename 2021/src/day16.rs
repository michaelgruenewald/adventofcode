use std::cmp::Ordering;

#[derive(Debug)]
enum Packet {
    Literal(u64, u64),
    Operator(u64, u64, Vec<Packet>),
}

use Packet::*;

impl Packet {
    fn version_sum(&self) -> u64 {
        match self {
            Literal(version, _) => *version,
            Operator(version, _, packets) => {
                *version + packets.iter().map(|p| p.version_sum()).sum::<u64>()
            }
        }
    }

    fn evaluate(&self) -> u64 {
        match self {
            Literal(_, value) => *value,
            Operator(_, 0, packets) => packets.iter().map(|p| p.evaluate()).sum(),
            Operator(_, 1, packets) => packets.iter().map(|p| p.evaluate()).product(),
            Operator(_, 2, packets) => packets.iter().map(|p| p.evaluate()).min().unwrap(),
            Operator(_, 3, packets) => packets.iter().map(|p| p.evaluate()).max().unwrap(),
            Operator(_, op @ (5 | 6 | 7), packets) => {
                let r = packets[0].evaluate().cmp(&packets[1].evaluate());
                if *op == 5 && r == Ordering::Greater
                    || *op == 6 && r == Ordering::Less
                    || *op == 7 && r == Ordering::Equal
                {
                    1
                } else {
                    0
                }
            }
            Operator(_, _, _) => unimplemented!(),
        }
    }
}

fn nextn(it: &mut dyn Iterator<Item = u8>, n: usize) -> u64 {
    (0..n)
        .map(|_| it.next().unwrap())
        .fold(0, |a, e| a * 2 + e as u64)
}

fn read(it: &mut dyn Iterator<Item = u8>) -> Packet {
    let version = nextn(it, 3);
    match nextn(it, 3) {
        4 => {
            let mut v = 0;
            loop {
                let more = nextn(it, 1);
                let num = nextn(it, 4);
                v = v * 16 + num;

                if more == 0 {
                    break;
                }
            }
            Literal(version, v)
        }
        packet_type => {
            let length_type = nextn(it, 1);
            match length_type {
                0 => {
                    let total_length = nextn(it, 15);
                    let mut sub = it.take(total_length as usize).peekable();
                    let mut packets = vec![];
                    while sub.peek().is_some() {
                        packets.push(read(&mut sub));
                    }
                    Operator(version, packet_type, packets)
                }
                1 => {
                    let sub_packet_count = nextn(it, 11);
                    Operator(
                        version,
                        packet_type,
                        (0..sub_packet_count).map(|_| read(it)).collect(),
                    )
                }
                _ => unreachable!(),
            }
        }
    }
}

fn bitstream(input: &str) -> impl Iterator<Item = u8> + '_ {
    input.chars().flat_map(|c| {
        (0..=3)
            .rev()
            .map(move |b| (c.to_digit(16).unwrap() & (1 << b)).clamp(0, 1) as u8)
    })
}

pub fn part1(input: &str) -> u64 {
    read(&mut bitstream(input)).version_sum()
}

pub fn part2(input: &str) -> u64 {
    read(&mut bitstream(input)).evaluate()
}

#[test]
fn test_part1() {
    assert_eq!(16, part1("8A004A801A8002F478"));
    assert_eq!(12, part1("620080001611562C8802118E34"));
    assert_eq!(23, part1("C0015000016115A2E0802F182340"));
    assert_eq!(31, part1("A0016C880162017C3686B18A3D4780"));
}

#[test]
fn test_part2() {
    assert_eq!(3, part2("C200B40A82"));
    assert_eq!(54, part2("04005AC33890"));
    assert_eq!(7, part2("880086C3E88112"));
    assert_eq!(9, part2("CE00C43D881120"));
    assert_eq!(1, part2("D8005AC2A8F0"));
    assert_eq!(0, part2("F600BC2D8F"));
    assert_eq!(0, part2("9C005AC2F8F0"));
    assert_eq!(1, part2("9C0141080250320F1802104A08"));
}
