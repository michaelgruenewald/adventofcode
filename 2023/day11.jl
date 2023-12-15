using Combinatorics: combinations
using Test

function part1(lines)
    m = reduce(vcat, permutedims.(collect.(lines)))

    expand_rows = [i for (i, r) in enumerate(eachrow(m)) if all(==('.'), r)]
    expand_cols = [i for (i, r) in enumerate(eachcol(m)) if all(==('.'), r)]

    sum(combinations(findall(==('#'), m), 2)) do (a, b)
        rows = min(a[1], b[1]):max(a[1], b[1])-1
        cols = min(a[2], b[2]):max(a[2], b[2])-1
        sum(r -> r ∈ expand_rows ? 2 : 1, rows, init=0) + sum(c -> c ∈ expand_cols ? 2 : 1, cols, init=0)
    end
end

function part2(lines)
    m = reduce(vcat, permutedims.(collect.(lines)))

    expand_rows = [i for (i, r) in enumerate(eachrow(m)) if all(==('.'), r)]
    expand_cols = [i for (i, r) in enumerate(eachcol(m)) if all(==('.'), r)]

    sum(combinations(findall(==('#'), m), 2)) do (a, b)
        rows = min(a[1], b[1]):max(a[1], b[1])-1
        cols = min(a[2], b[2]):max(a[2], b[2])-1
        sum(r -> r ∈ expand_rows ? 1_000_000 : 1, rows, init=0) + sum(c -> c ∈ expand_cols ? 1_000_000 : 1, cols, init=0)
    end
end

function run()
    input = readlines("input11.txt")
    println(part1(input))
    println(part2(input))
end

function test()
    @testset "Tests" begin
        example = [
            "...#......",
            ".......#..",
            "#.........",
            "..........",
            "......#...",
            ".#........",
            ".........#",
            "..........",
            ".......#..",
            "#...#.....",
        ]

        @test part1(example) == 374
    end
end

if ARGS == ["test"]
    test()
else
    run()
end
