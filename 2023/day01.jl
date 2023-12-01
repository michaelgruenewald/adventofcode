using Match
using Test

function part1(lines)
    sum(map(line -> parse(Int, string(line[findfirst(isdigit, line)], line[findlast(isdigit, line)])), lines))
end

function part2(lines)
    sum(map(lines) do line
        matches = collect(eachmatch(r"(\d)|(one|two|three|four|five|six|seven|eight|nine)", line, overlap=true))
        foldl((l, r) -> l * 10 + r, map(matches[[begin, end]]) do m
            @match m.captures begin
                [d, ::Nothing] => parse(Int, d)
                [::Nothing, word] => findfirst(==(word), ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"])
            end
        end)
    end)
end

function run()
    input = collect(eachline(open("input01.txt")))
    println(part1(input))
    println(part2(input))
end

function test()
    @testset "Tests" begin
        @test part1([
            "1abc2",
            "pqr3stu8vwx",
            "a1b2c3d4e5f",
            "treb7uchet",
        ]) == 142

        @test part2([
            "two1nine",
            "eightwothree",
            "abcone2threexyz",
            "xtwone3four",
            "4nineeightseven2",
            "zoneight234",
            "7pqrstsixteen",
        ]) == 281
    end
end

if ARGS == ["test"]
    test()
else
    run()
end
