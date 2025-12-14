local file = io.open("1/input.txt", "r")

if file == nil then
	return
end

local rotation = 50
local result = 0

for line in file:lines() do
	local distance = line:sub(2, -1)

	if line:sub(1, 1) == "L" then
		rotation = (rotation - tonumber(distance)) % 100
	else
		rotation = (rotation + tonumber(distance)) % 100
	end

	if rotation == 0 then
		result = result + 1
	end
end

print(result)
