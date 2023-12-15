using Base.Iterators: countfrom
using Test

const NEIGHBOURS = [
    CartesianIndex(-1, 0), CartesianIndex(+1, 0), CartesianIndex(0, -1), CartesianIndex(0, +1)
]

const DIRECTIONS = Dict(
    '|' => Set([CartesianIndex(-1, 0), CartesianIndex(+1, 0)]),
    '-' => Set([CartesianIndex(0, -1), CartesianIndex(0, +1)]),
    'L' => Set([CartesianIndex(-1, 0), CartesianIndex(0, +1)]),
    'J' => Set([CartesianIndex(-1, 0), CartesianIndex(0, -1)]),
    '7' => Set([CartesianIndex(+1, 0), CartesianIndex(0, -1)]),
    'F' => Set([CartesianIndex(+1, 0), CartesianIndex(0, +1)]),
    '.' => Set(),
)
const INV_DIRECTIONS = Dict(reverse(p) for p in DIRECTIONS)

function part1(lines)
    m = reduce(vcat, permutedims.(collect.(lines)))
    start = findfirst(==('S'), m)
    start_neighbours = filter(p -> checkbounds(Bool, m, p) && any(==(start), DIRECTIONS[m[p]] .+ (p,)), NEIGHBOURS .+ (start,))
    m[start] = INV_DIRECTIONS[Set(start_neighbours .- (start,))]

    last_pos, pos = nothing, start
    for step in countfrom(1)
        last_pos, pos = pos, (first ∘ filter)(p -> p != last_pos, DIRECTIONS[m[pos]] .+ (pos,))
        if pos == start
            return step ÷ 2
        end
    end
end

function part2(lines)
    m = reduce(vcat, permutedims.(collect.(lines)))
    start = findfirst(==('S'), m)
    start_neighbours = filter(p -> checkbounds(Bool, m, p) && any(==(start), DIRECTIONS[m[p]] .+ (p,)), NEIGHBOURS .+ (start,))
    m[start] = INV_DIRECTIONS[Set(start_neighbours .- (start,))]

    last_pos, pos = nothing, start
    seen = zeros(Bool, size(m))
    while !seen[pos]
        seen[pos] = true
        last_pos, pos = pos, (first ∘ filter)(p -> p != last_pos, DIRECTIONS[m[pos]] .+ (pos,))
    end

    tiles = 0
    for p in vcat(CartesianIndices((1:size(m, 1), 1))..., CartesianIndices((1, 2:size(m, 2)))...)
        inside = false
        while checkbounds(Bool, m, p)
            if seen[p] && m[p] ∈ ('|', '-', 'F', 'J')
                inside = !inside
            end
            if inside && !seen[p]
                tiles += 1
            end
            p += CartesianIndex(1, 1)
        end
    end

    tiles
end

function run()
    input = readlines("input10.txt")
    println(part1(input))
    println(part2(input))
end

function test()
    @testset "Tests" begin
        example1 = [
            "-L|F7",
            "7S-7|",
            "L|7||",
            "-L-J|",
            "L|-JF",
        ]
        example2 = [
            "7-F7-",
            ".FJ|7",
            "SJLL7",
            "|F--J",
            "LJ.LJ",
        ]
        example3 = [
            "...........",
            ".S-------7.",
            ".|F-----7|.",
            ".||.....||.",
            ".||.....||.",
            ".|L-7.F-J|.",
            ".|..|.|..|.",
            ".L--J.L--J.",
            "...........",
        ]
        example4 = [
            ".F----7F7F7F7F-7....",
            ".|F--7||||||||FJ....",
            ".||.FJ||||||||L7....",
            "FJL7L7LJLJ||LJ.L-7..",
            "L--J.L7...LJS7F-7L7.",
            "....F-J..F7FJ|L7L7L7",
            "....L7.F7||L7|.L7L7|",
            ".....|FJLJ|FJ|F7|.LJ",
            "....FJL-7.||.||||...",
            "....L---J.LJ.LJLJ...",
        ]
        example5 = [
            "FF7FSF7F7F7F7F7F---7",
            "L|LJ||||||||||||F--J",
            "FL-7LJLJ||||||LJL-77",
            "F--JF--7||LJLJ7F7FJ-",
            "L---JF-JLJ.||-FJLJJ7",
            "|F|F-JF---7F7-L7L|7|",
            "|FFJF7L7F-JF7|JL---7",
            "7-L-JL7||F7|L7F-7F7|",
            "L.L7LFJ|||||FJL7||LJ",
            "L7JLJL-JLJLJL--JLJ.L",
        ]

        @test part1(example1) == 4
        @test part1(example2) == 8
        @test part2(example3) == 4
        @test part2(example4) == 8
        @test part2(example5) == 10
    end
end

if ARGS == ["test"]
    test()
else
    run()
end
