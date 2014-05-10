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
print([[
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
]])
--map based area
item = {
[1] = "rupee",
[2] = "heart",
[3] = "sword",
[4] = "sword",--"realsword",
[5] = "hole",
[6] = "monster",
[7] = "portal"
}
--read map using readmap from T1



for line in io.lines("items.log") do 
	_,_,x,y,item = string.find(line, "(.+)%s(.+)%s(.)")
	print(string.format("%s(%s,%s).",item,x,y))
end

--do the same for terrain
