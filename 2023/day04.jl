using Test

function part1(lines)
    sum(lines) do line
        (winning, own) = match(r"Card *\d+: (.*) [|] (.*)", line)
        1 << length(intersect(split(winning), split(own))) >> 1
    end
end

function part2(lines)
    matches = map(lines) do line
        (winning, own) = match(r"Card *\d+: (.*) [|] (.*)", line)
        length(intersect(split(winning), split(own)))
    end

    cards = fill(1, length(matches))
    for (i, m) in enumerate(matches)
        cards[i+1:min(i+m, length(cards))] .+= cards[i]
    end
    sum(cards)
end

function run()
    input = collect(eachline(open("input04.txt")))
    println(part1(input))
    println(part2(input))
end

function test()
    @testset "Tests" begin
        example = [
            "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
            "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
            "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
            "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
            "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
            "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11",
        ]

        @test part1(example) == 13
        @test part2(example) == 30
    end
end

if ARGS == ["test"]
    test()
else
    run()
end
