using Test

function next_num(nums)
    nums[end] + (allequal(nums) ? 0 : next_num(diff(nums)))
end

function part1(lines)
    sum(lines) do line
        next_num(parse.(Int, split(line)))
    end
end

function prev_num(nums)
    nums[begin] - (allequal(nums) ? 0 : prev_num(diff(nums)))
end

function part2(lines)
    sum(lines) do line
        prev_num(parse.(Int, split(line)))
    end
end

function run()
    input = readlines("input09.txt")
    println(part1(input))
    println(part2(input))
end

function test()
    @testset "Tests" begin
        example = [
            "0 3 6 9 12 15",
            "1 3 6 10 15 21",
            "10 13 16 21 30 45",
        ]

        @test part1(example) == 114
        @test part2(example) == 2
    end
end

if ARGS == ["test"]
    test()
else
    run()
end
