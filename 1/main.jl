elves = []

open("input.txt") do stream
	for chunk in collect(eachsplit(read(stream, String), "\n\n"))
		sum = 0
		for line in collect(eachsplit(chunk, "\n"))
			sum += line == "" ? 0 : parse(Int64, line)
		end
		push!(elves, sum)
	end
end

# part 1
println("part 1: ", maximum(elves))

# part 2
elves = sort(elves, rev=true)
println("part 2: ", sum(elves[1:3]))
