using Test
using OffsetArrays: Origin

function part1(lines)
    it = Iterators.Stateful(lines)
    (m,) = match(r"seeds: (.*)", popfirst!(it))
    seeds = parse.(Int, split(m))
    @assert isempty(popfirst!(it))

    while true
        ranges = map(Iterators.drop(Iterators.takewhile((!) ∘ isempty, it), 1)) do line
            parse.(Int, split(line))
        end
        if isempty(ranges)
            break
        end
        seeds = map(seeds) do seed
            for (dest_start, source_start, len) ∈ ranges
                if source_start ≤ seed ≤ source_start + len
                    seed = seed - source_start + dest_start
                    break
                end
            end
            seed
        end
    end

    min(seeds...)
end

function part2(lines)
    it = Iterators.Stateful(lines)
    (m,) = match(r"seeds: (.*)", popfirst!(it))
    @assert isempty(popfirst!(it))

    seeds = map(Iterators.partition(parse.(Int, split(m)), 2)) do (start, length)
        start:start+length-1
    end

    # the correct version would be to use a run-length-encoded vector here, but memory is cheap, eh?
    v = Origin(0)(BitVector(undef, maximum(r -> r.stop, seeds) + 1))
    for seed ∈ seeds
        v[seed] .= true
    end

    while true
        ranges = map(line -> parse.(Int, split(line)), Iterators.drop(Iterators.takewhile((!) ∘ isempty, it), 1))
        if isempty(ranges)
            break
        end

        v_new = deepcopy(v)
        for (dest_start, source_start, range_length) ∈ ranges
            if lastindex(v_new) < dest_start + range_length - 1
                resize!(v_new, dest_start + range_length)
            end
            for i ∈ 1:range_length
                v_new[dest_start-1+i] = get(v, source_start - 1 + i, false)
            end
        end
        v = v_new
    end

    findfirst(v)
end

function run()
    input = readlines("input05.txt")
    println(part1(input))
    println(part2(input))
end

function test()
    @testset "Tests" begin
        example = [
            "seeds: 79 14 55 13",
            "",
            "seed-to-soil map:",
            "50 98 2",
            "52 50 48",
            "",
            "soil-to-fertilizer map:",
            "0 15 37",
            "37 52 2",
            "39 0 15",
            "",
            "fertilizer-to-water map:",
            "49 53 8",
            "0 11 42",
            "42 0 7",
            "57 7 4",
            "",
            "water-to-light map:",
            "88 18 7",
            "18 25 70",
            "",
            "light-to-temperature map:",
            "45 77 23",
            "81 45 19",
            "68 64 13",
            "",
            "temperature-to-humidity map:",
            "0 69 1",
            "1 0 69",
            "",
            "humidity-to-location map:",
            "60 56 37",
            "56 93 4",
        ]

        @test part1(example) == 35
        @test part2(example) == 46
    end
end

if ARGS == ["test"]
    test()
else
    run()
end
