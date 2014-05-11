require ("readmap")

item_id = {
["R"] = "rupee_glow",
["C"] = "fairies",
["F"] = "pendants_glow",
["M"] = "pendants_glow",
["B"] = "breeze",
["E"] = "noises",
["V"] = "spatial_distortions"
}
map = read_map("terreno.txt")
io.output("src/prolog/info.log")

for x=1,#map do
	for y=1,#map[1] do
 		if (map[x][y] == "1") then io.write(string.format("pos(%s,%s).",x,y)) end
	end
end
for line in io.lines("items.log") do 
	_,_,x,y,item_tmp = string.find(line, "(.+)%s(.+)%s(.)")
	if item_id =="B" or item_id =="E" or item_id =="V" then 
		io.write(string.format("%s(%s,%s).",item_id[item_tmp],x+1,y+1)) 
		io.write(string.format("%s(%s,%s).",item_id[item_tmp],x+1,y-1)) 
		io.write(string.format("%s(%s,%s).",item_id[item_tmp],x-1,y+1)) 
		io.write(string.format("%s(%s,%s).",item_id[item_tmp],x-1,y-1)) 

	else 
		io.write(string.format("%s(%s,%s).",item_id[item_tmp],x,y))
	end
end

--do the same for terrain
