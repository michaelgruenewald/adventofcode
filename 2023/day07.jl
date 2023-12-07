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
        by_type = findfirst(==(cardinalities), [
            Multiset(5),
            Multiset(1, 4),
            Multiset(2, 3),
            Multiset(1, 1, 3),
            Multiset(1, 2, 2),
            Multiset(1, 1, 1, 2),
            Multiset(1, 1, 1, 1, 1),
        ])

        by_cards = map(c -> findfirst(c, "AKQJT98765432"), collect(hand))
        (by_type, by_cards)
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

        by_type = findfirst(==(cardinalities), [
            Multiset(5 - jokers),
            Multiset(), # just jokers is the worst five of a kind
            Multiset(1, 4 - jokers),
            Multiset(2, 3 - jokers),
            Multiset(1, 1, 3 - jokers),
            Multiset(1, 2, 2 - jokers),
            Multiset(1, 1, 1, 2 - jokers),
            Multiset(1, 1, 1, 1, 1),
        ])

        by_cards = map(c -> findfirst(c, "AKQT98765432J"), collect(hand))
        (by_type, by_cards)
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
