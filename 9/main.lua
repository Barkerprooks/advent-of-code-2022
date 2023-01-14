#!/usr/bin/env lua

function sleep (n)
	local delta = tonumber(os.clock() + n)
	while (os.clock() < delta) do end
end

function lines (str)
	local lines = {}

	for line in str:gmatch("[^\r\n]+") do
		table.insert(lines, line)
	end

	return lines
end

function readlines (path)
	local file = assert(io.open(path, "r"))
	local text = file:read("*all")

	file:close()

	return lines(text)
end

function updateY (y, o)
	if o == "U" then
		return y - 1
	elseif o == "D" then
		return y + 1
	end
	return y
end

function updateX (x, o)
	if o == "L" then
		return x - 1
	elseif o == "R" then
		return x + 1
	end
	return x
end

function makeGrid (w, h)
	local grid = {}
	for y=1,h do
		for x=1,w do
			grid[x + w * y] = '.'
		end
	end
	return grid
end

function showGrid (grid, w, h)
	for y=1,h do
		for x=1,w do
			io.stdout:write(grid[x + w * y])
		end
		print()
	end
	print()
end

function showGridPos (grid, w, h, x, y)
	for yp=1,h do
		for xp=1,w do
			if xp == x and yp == y then
				io.stdout:write('#')
			else
				io.stdout:write(grid[x + w * y])
			end
		end
		print()
	end
	print()
end

function parseInput (path)
	local x, y, xs, ys, cells = 1, 1, {1}, {1}, {{x=1, y=1}}
	
	for i, line in pairs(readlines(path)) do
		o, n = line:sub(1, 1), tonumber(line:sub(3, line:len()))
		x, y = updateX(x, o), updateY(y, o)
		table.insert(xs, x)
		table.insert(ys, y)
		table.insert(cells, {x=x, y=y})
	end

	return cells, xs, ys
end

cells, xs, ys = parseInput("input.txt")

minX, minY = math.min(table.unpack(xs)), math.min(table.unpack(ys))
maxX, maxY = math.max(table.unpack(xs)), math.max(table.unpack(ys))
w, h = math.abs(minX) + maxX + 1, math.abs(minY) + maxY + 1

grid = makeGrid(w, h)
for i, cell in pairs(cells) do
	local x, y = cell.x + math.abs(minX) + 1, cell.y + math.abs(minY) + 1
	grid[x + w * y] = '*'
	showGridPos(grid, w, h, x, y)
	sleep(0.1)
end
