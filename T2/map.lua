-- creates a random map 

--terrain = readmap(terrain)

m = 42
n = 42
map = {}--represents the items
for i=0,m do
	map[i] = {}
	for j=0,n do
		map[i][j] = math.random(1,7)
	end
end

--do it according to file, limits and checking grass	
