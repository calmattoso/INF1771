
function read_map(path)
file = io.open(path)
text = file:read("*all")
file:close(path)


local map = {}
local width = 42
local i = 1
local j = 1
map[i] = {}
text = text:gsub("%s","")
for l in string.gmatch(text, "%L") do
  map[i][j] = l
  j = j+1
  if j == width then
    i = i+1
    j = 1
    map[i] = {}
  end
end

return map 

end
