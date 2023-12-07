using Multisets
using Test

function part1(lines)
    hands = map(lines) do line
        hand, bid_s = split(line)
        (hand, parse(Int, bid_s))
    end

    ordered = sort(hands, rev=true, by=((hand, _),) -> begin
        cards = Multiset(hand...)
        cardinalities = Multiset(values(cards)...)
        rank = if cardinalities == Multiset(5)
            1
        elseif cardinalities == Multiset(1, 4)
            2
        elseif cardinalities == Multiset(2, 3)
            3
        elseif cardinalities == Multiset(1, 1, 3)
            4
        elseif cardinalities == Multiset(1, 2, 2)
            5
        elseif cardinalities == Multiset(1, 1, 1, 2)
            6
        elseif cardinalities == Multiset(1, 1, 1, 1, 1)
            7
        else
            @assert false
        end

        second = map(c -> findfirst(c, "AKQJT98765432"), collect(hand))
        (rank, second)
    end)

    sum(enumerate(ordered)) do (rank, (_, bid))
        rank * bid
    end
end

function part2(lines)
    hands = map(lines) do line
        hand, bid_s = split(line)
        (hand, parse(Int, bid_s))
    end

    ordered = sort(hands, rev=true, by=((hand, _),) -> begin
        cards = Multiset(hand...)

        # put the jokers aside
        jokers = cards['J']
        delete!(cards, 'J')

        cardinalities = Multiset(values(cards)...)
        rank = if jokers == 5 || cardinalities == Multiset(5 - jokers)
            1
        elseif cardinalities == Multiset(1, 4 - jokers)
            2
        elseif cardinalities == Multiset(2, 3 - jokers)
            3
        elseif cardinalities == Multiset(1, 1, 3 - jokers)
            4
        elseif cardinalities == Multiset(1, 2, 2 - jokers)
            5
        elseif cardinalities == Multiset(1, 1, 1, 2 - jokers)
            6
        elseif cardinalities == Multiset(1, 1, 1, 1, 1)
            7
        else
            @assert false
        end

        second = map(c -> findfirst(c, "AKQT98765432J"), collect(hand))
        (rank, second)
    end)

    sum(enumerate(ordered)) do (rank, (_, bid))
        rank * bid
    end
end

function run()
    input = readlines("input07.txt")
    println(part1(input))
    println(part2(input))
end

function test()
    @testset "Tests" begin
        example = [
            "32T3K 765",
            "T55J5 684",
            "KK677 28",
            "KTJJT 220",
            "QQQJA 483",
        ]

        @test part1(example) == 6440
        @test part2(example) == 5905
    end
end

if ARGS == ["test"]
    test()
else
    run()
end
