--reads a map and make a series of prolog statements based on the map

--static area (will not changed based on the map values)

--link actions, sorted by priority
--[[
se tem rupee, pega.
se tem espada, pega
se tem coracao e energia<50, pega.	
se tem monstro adjacente e energia maior que 10, ataca.
se em frente estiver seguro, anda.
vira.
--]]
print("
best_action(X,Y,FX,FY,Energy) :- pickup_rupee(X,Y).
best_action(X,Y,FX,FY,Energy) :- pickup_sword(X,Y).
best_action(X,Y,FX,FY,Energy) :- pickup_heart(X,Y), Energy<50.
best_action(X,Y,FX,FY,Energy) :- atack(FX,FY), Energy>10.
best_action(X,Y,FX,FY,Energy) :- walk(FX,FY).
best_action(X,Y,Energy) :- turn_right().

%best_action(X,Y,Energy) :- facing(X+1,Y), atack(X+1,Y), Energy>10.
%best_action(X,Y,Energy) :- facing(X-1,Y), atack(X-1,Y), Energy>10.
%best_action(X,Y,Energy) :- facing(X,Y+1), atack(X,Y+1), Energy>10.
%best_action(X,Y,Energy) :- facing(X,Y-1), atack(X,Y-1), Energy>10.

%best_action(X,Y,Energy) :- facing(X+1,Y), safe(X+1,Y).
%best_action(X,Y,Energy) :- facing(X-1,Y), safe(X-1,Y).
%best_action(X,Y,Energy) :- facing(X,Y+1), safe(X,Y+1).
%best_action(X,Y,Energy) :- facing(X,Y-1), safe(X,Y-1).


walk() :- muda x = fx ,y = fy
turn_right() :- cicla entre os 4 estados
turn_left() :-
atack(X,Y) :- 
pickup_heart(X,Y) :- heart(X,Y).
pickup_rupee(X,Y) :- rupee(X,Y).
pickup_sword(X,Y) :- sword(X,Y).
")
--map based area
item = {
[1] = "rupee",
<<<<<<< HEAD
[2] = "sword",
=======
[2] = "real_sword",
[3] = "fake_sword",
>>>>>>> 421daaee524619c6450df68b6f1f4aac9b4dfa63
[4] = "heart",
[4] = "sword",--"realsword",
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
