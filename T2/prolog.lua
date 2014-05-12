require ("readmap")
require ("prolog_aux")
sensor_id = {
["R"] = "rupee_glow",
["C"] = "fairies",
["F"] = "pendants_glow",
["M"] = "pendants_glow",
["B"] = "breeze",
["E"] = "noises",
["V"] = "spatial_distortions"
}
item_id = {
["M"] = "master_sword",
["B"] = "hole",
["E"] = "monster",
["V"] = "vortex"
}
map = read_map("data/map.txt")
io.output("src/prolog/problem.pl")

io.write(header)

for x=1,#map do
	for y=1,#map[1] do
 		if (map[x][y] == "1") then io.write(string.format("pos(%s,%s).",x,y)) end
	end
end
for line in io.lines("items.log") do 
	_,_,x,y,item_tmp = string.find(line, "(%d+)%s(%d+)%s(%a)")
	if item_temp =="B" or item_temp =="E" or item_temp =="V" then 
			if x < 42 then
				if y < 42 then io.write(string.format("%s(%s,%s).\n",sensor_id[item_tmp],x+1,y+1)) end
				if x>0 then io.write(string.format("%s(%s,%s).\n",sensor_id[item_tmp],x+1,y-1)) end
			end
			if x>0 then
				if y < 42 then io.write(string.format("%s(%s,%s).\n",sensor_id[item_tmp],x-1,y+1)) end
				if x>0 then io.write(string.format("%s(%s,%s).\n",sensor_id[item_tmp],x-1,y-1)) end
			end
	else 
		io.write(string.format("at(%s,pos(%s,%s)).\n",sensor_id[item_tmp],x,y))
	end
	if item_temp =="B" or item_temp =="E" or item_temp =="V" or item_temp =="M" then 
		io.write(string.format("at(%s,pos(%s,%s)).\n",item_id[item_tmp],x,y)) 
	end
end

io.write(footer)