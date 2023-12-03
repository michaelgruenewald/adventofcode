using ImageFiltering
using Test

function part1(lines)
    # turn the lines into a matrix
    m = reduce(vcat, permutedims.(collect.(lines)))
    # make a bool matrix of all the digits
    digits = map(∈("0123456789"), m)
    # and another bool matrix of all the symbols
    syms = map(∉(".0123456789"), m)
    # and a matrix with all the positions reachable by a symbol
    adjsyms = Bool.(clamp.(imfilter(syms, centered(ones(Int, 3, 3))), 0, 1))

    # for all the digits in a reachable place
    numbers = Set(map(findall(digits .&& adjsyms)) do idx
        # find the entire number
        union(
            Set(collect(Iterators.takewhile(i -> digits[i], idx:CartesianIndex(1, -1):CartesianIndex(idx[1], 1)))),
            Set(collect(Iterators.takewhile(i -> digits[i], idx:CartesianIndex(idx[1], size(m, 2)))))
        )
    end)

    # convert the number coords into actual numbers and add them up
    sum(numbers) do coords
        foldl((l, rc) -> l * 10 + parse(Int, m[rc]), sort(collect(coords)), init=0)
    end
end

function part2(lines)
    # turn the lines into a matrix
    m = reduce(vcat, permutedims.(collect.(lines)))
    # make a bool matrix of all the digits
    digits = map(∈("0123456789"), m)

    # for each gear, sum up ...
    sum(findall(map(==('*'), m))) do sym
        # make a matrix with all the positions around the gear being true
        reachable = zeros(Bool, size(m)...)
        reachable[sym] = true
        adjsyms = Bool.(clamp.(imfilter(reachable, centered(ones(Int, 3, 3))), 0, 1))

        # for all the digits in a reachable place
        numbers = Set(map(findall(digits .&& adjsyms)) do idx
            # find the entire number
            union(
                Set(collect(Iterators.takewhile(i -> digits[i], idx:CartesianIndex(1, -1):CartesianIndex(idx[1], 1)))),
                Set(collect(Iterators.takewhile(i -> digits[i], idx:CartesianIndex(idx[1], size(m, 2)))))
            )
        end)

        # if we have two numbers, return the gear ratio, otherwise zero
        if length(numbers) == 2
            prod(numbers) do coords
                foldl((l, rc) -> l * 10 + parse(Int, m[rc]), sort(collect(coords)), init=0)
            end
        else
            0
        end
    end
end

function run()
    input = collect(eachline(open("input03.txt")))
    println(part1(input))
    println(part2(input))
end

function test()
    @testset "Tests" begin
        example = [
            raw"467..114..",
            raw"...*......",
            raw"..35..633.",
            raw"......#...",
            raw"617*......",
            raw".....+.58.",
            raw"..592.....",
            raw"......755.",
            raw"...$.*....",
            raw".664.598..",
        ]

        @test part1(example) == 4361
        @test part2(example) == 467835
    end
end

if ARGS == ["test"]
    test()
else
    run()
end
