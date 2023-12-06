using Test

xceil(x) = ceil(nextfloat(x))
xfloor(x) = floor(prevfloat(x))

function part1(lines)
    times = map(m -> parse(Int, m.match), eachmatch(r"\d+", lines[1]))
    distances = map(m -> parse(Int, m.match), eachmatch(r"\d+", lines[2]))

    prod(zip(times, distances)) do (t, d)
        length(xceil(-(√(t * t - 4 * d) - t) / 2):xfloor((√(t * t - 4 * d) + t) / 2))
    end
end

function part2(lines)
    t = parse(Int, replace(lines[1], r"\D+" => ""))
    d = parse(Int, replace(lines[2], r"\D+" => ""))

    length(xceil(-(√(t * t - 4 * d) - t) / 2):xfloor((√(t * t - 4 * d) + t) / 2))
end

function run()
    input = readlines("input06.txt")
    println(part1(input))
    println(part2(input))
end

function test()
    @testset "Tests" begin
        example = [
            "Time:      7  15   30",
            "Distance:  9  40  200",
        ]

        @test part1(example) == 288
        @test part2(example) == 71503
    end
end

if ARGS == ["test"]
    test()
else
    run()
end
