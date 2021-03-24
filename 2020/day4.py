#!/usr/bin/env python
import re

lines = """\
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
"""

lines = open("input4.txt").read()

passports = [
    {k: v for k, _, v in (d.partition(":") for d in passport.split())}
    for passport in lines.split("\n\n")
]

# part 1
required = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
print(sum(required.issubset(p.keys()) for p in passports))

# part 2
rules = {
    "byr": r"19[2-9][0-9]|200[0-2]",
    "iyr": r"201[0-9]|2020",
    "eyr": r"202[0-9]|2030",
    "hgt": r"1[5-8][0-9]cm|19[0-3]cm|59in|6[0-9]in|7[0-6]in",
    "hcl": r"#[0-9a-f]{6}",
    "ecl": r"amb|blu|brn|gry|grn|hzl|oth",
    "pid": r"[0-9]{9}",
}
print(
    sum(
        all(f in p and re.fullmatch(e, p[f]) for f, e in rules.items())
        for p in passports
    )
)
