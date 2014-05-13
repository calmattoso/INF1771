
function read_map(path)
file = io.open(path)
text = file:read("*all")
file:close(path)


local map = {}
local width = 43
local i = 1
local j = 1
map[i] = {}
text = text:gsub("%s+","")
for l in string.gmatch(text, "%l") do
  if l == "g" then
  	map[i][j] = 1
  else 
  	map[i][j] = 2
  end
  j = j+1
  if j == width then
    i = i+1
    j = 1
    map[i] = {}
  end
end

return map 

end
