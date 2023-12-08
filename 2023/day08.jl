using .Iterators: cycle
using Test

function part1(lines)
    network = Dict(map(lines[3:end]) do line
        from, left, right = match(r"(\w+) = \((\w+), (\w+)\)", line)
        (from, Dict('L' => left, 'R' => right))
    end)

    current = "AAA"
    for (step, i) in enumerate(cycle(lines[begin]))
        current = network[current][i]
        if current == "ZZZ"
            return step
        end
    end
end

function part2(lines)
    network = Dict(map(lines[3:end]) do line
        from, left, right = match(r"(\w+) = \((\w+), (\w+)\)", line)
        (from, Dict('L' => left, 'R' => right))
    end)

    initial = collect(filter(node -> node[end] == 'A', keys(network)))

    (lcm ∘ map)(initial) do current
        for (step, i) in enumerate(cycle(lines[begin]))
            current = network[current][i]
            if current[end] == 'Z'
                return step
            end
        end
    end
end

function run()
    input = readlines("input08.txt")
    println(part1(input))
    println(part2(input))
end

function test()
    @testset "Tests" begin
        example1 = [
            "RL",
            "",
            "AAA = (BBB, CCC)",
            "BBB = (DDD, EEE)",
            "CCC = (ZZZ, GGG)",
            "DDD = (DDD, DDD)",
            "EEE = (EEE, EEE)",
            "GGG = (GGG, GGG)",
            "ZZZ = (ZZZ, ZZZ)",
        ]
        example2 = [
            "LLR",
            "",
            "AAA = (BBB, BBB)",
            "BBB = (AAA, ZZZ)",
            "ZZZ = (ZZZ, ZZZ)",
        ]
        example3 = [
            "LR",
            "",
            "11A = (11B, XXX)",
            "11B = (XXX, 11Z)",
            "11Z = (11B, XXX)",
            "22A = (22B, XXX)",
            "22B = (22C, 22C)",
            "22C = (22Z, 22Z)",
            "22Z = (22B, 22B)",
            "XXX = (XXX, XXX)",
        ]

        @test part1(example1) == 2
        @test part1(example2) == 6
        @test part2(example3) == 6
    end
end

if ARGS == ["test"]
    test()
else
    run()
end
