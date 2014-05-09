--reads a map and make a series of prolog statements based on the map

--static area (will not changed based on the map values)

--link actions, sorted by priority
print("
walk() :-
turn_right() :-
turn_left() :-
atack() :-
pickup_heart() :-
pickup_rupee() :-
pickup_sword() :-
")
--map based area
item = {
[1] = "rupee",
[2] = "real_sword",
[3] = "fake_sword",
[4] = "heart",
[5] = "hole",
[6] = "monster",
[7] = "portal"
}

--read map using readmap from T1
m = #map
n = #map[1]
mapsize = m*n
for i=0,mapsize do
	x = i % m
	y = i / n
	prolog_o = string.format("%s(%d,%d).", item[map[x][y]],x,y)
	print(prolog_o)
end

--do the same for terrain
