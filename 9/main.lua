#!/usr/bin/env lua

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

function updateY (y, op)
	if o == "U" then
		return y - 1
	elseif o == "D" then
		return y + 1
	end
	return y
end

function updateX (x, op)
	if o == "L" then
		return x - 1
	elseif o == "R" then
		return x + 1
	end
	return x
end

x, y, xs, ys = 0, 0, {0}, {0}
for i, line in pairs(readlines("input.txt")) do
	o, n = line:sub(1, 1), tonumber(line:sub(3, line:len()))
	x, y = updateX(x, o), updateY(y, o)
	table.insert(xs, x)
	table.insert(ys, y)
end

minx, miny = math.min(table.unpack(xs)), math.min(table.unpack(ys))
maxx, maxy = math.max(table.unpack(xs)), math.max(table.unpack(ys))
w, h = (-1 * minx) + maxx, (-1 * miny) + maxy
