using Test

function part1(lines)
    MAX_CUBES = Dict("red" => 12, "green" => 13, "blue" => 14)

    sum(lines) do line
        game_id, rest = match(r"Game (\d+): (.*)", line)
        good = all(split(rest, "; ")) do s
            all(split(s, r", ")) do m
                (count, color) = split(m, " ")
                parse(Int, count) ≤ MAX_CUBES[color]
            end
        end

        good * parse(Int, game_id)
    end
end

function part2(lines)
    sum(lines) do line
        minimums = Dict()
        for (count, color) ∈ eachmatch(r"(\d+) (red|green|blue)", line)
            minimums[color] = max(get(minimums, color, 0), parse(Int, count))
        end
        prod(values(minimums))
    end
end

function run()
    input = collect(eachline(open("input02.txt")))
    println(part1(input))
    println(part2(input))
end

function test()
    @testset "Tests" begin
        example = [
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
        ]

        @test part1(example) == 8
        @test part2(example) == 2286
    end
end

if ARGS == ["test"]
    test()
else
    run()
end
