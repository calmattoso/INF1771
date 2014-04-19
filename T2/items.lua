-- creates a random map 

require("readmap")

terrain = read_map("terreno.txt")


--for i=1,#terrain do print(table.concat(terrain[i],"")) end

m = 42
n = 42
map = {}--represents the items
for i=1,m do
	map[i] = {}
end

item_id = {
[1] = "R",
[2] = "C",
[3] = "F",
[4] = "M",
[5] = "B",
[6] = "E",
[7] = "V"
}


--do it according to file, limits and checking grass	
function get_pos()
	x = math.random(1,m)
	y = math.random(1,n)
	return x,y
end

function validate(item,x,y)
	--item has to be on grass
	if terrain[x][y] == 1 then return false end
	--you cant have two of the same item in the same place
	if map[x][y] == item then
		return false
	end
	--hole, monster, portal and realsword cant share positions
	if item > 4 and map[x][y] and map[x][y] > 4 then 
		return false
	end
	return true
end

function create(item, num)
	for i=1,num do
		x,y = get_pos()
		while not (validate(item,x,y)) do
			x,y = get_pos()
		end
		map[x][y] = item
		io.write(x.." "..y.." "..item_id[item].."\n")
	end
end

math.randomseed(os.time())
io.output("items.log")
create(1,50) -- "rupee"
create(2,30) -- "heart"
create(3,30) -- "sword"
create(4,1) -- "realsword"
create(5,10) -- "hole"
create(6,100) -- "monster"
create(7,10) -- "portal"

