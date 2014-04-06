--format:
-- <info>\n\n<gates>\n\n<map>

--<info>: id n_gates width height 
--all ints
--<gates>: <gate>\n<gate>
--<gate>: x y dest_id
--<map>: height lines of width chars each

-- the value of each char can be looked up on terrain.txt

require("terrain")



function read_map(path,id)
file = io.open(path)



id = file:read("*num")
id = id+1
n_gates = file:read("*num")
width = file:read("*num")
height = file:read("*num")


local gates = {}
for i=1, n_gates do
  gates[i] = {}
  gates[i].x = file:read("*num")
  gates[i].y = file:read("*num")
  gates[i].dest = file:read("*num")
end

width = width + 1 
text = file:read("*all")

file:close(path)
local map = {}

local i = 1
local j = 1
map[i] = {}
text = text:gsub("%s","")
for l in string.gmatch(text, "%L") do
  map[i][j] = terrain[l]
  j = j+1
  if j == width then
    i = i+1
    j = 1
    map[i] = {}
  end
end

if id == 1 then
  map[7][6] = 7 --lost woods entrance
  costs[8] = costs[map[link.initial_y][link.initial_x]]
  map[link.initial_y][link.initial_x] = 8 --link's house
end
return map,gates 



end
