seq = {}
path = "../logs/solution.log"

--for a_star:
map = read_map() 
link_x =
link_y = 

function parse(line)
  _line = {}
  _,_,_line.action,_line.content,_line.x,_line.y = string.find(line, "(%a+)\((%d+),(%d+),(%d+)\)")
  return _line
end

function a_star(line)

  table.insert(seq,{"move",x,y})
end

for line in io.lines(path) do 
  _line = parse(line)
  -- usar http://lua-users.org/wiki/SwitchStatement ?
  if _line.action == "move_to" then
    if item[link_x][link_y] == "vortex" then
      table.insert(seq,{"teleport",_line.x,_line.y})
    else  
      a_star(_line.x,_line.y) 
  end

  if _line.action == "actual_danger" or _line.action == "item" then
    table.insert(seq,{_line.content,_line.x,_line.y})
  end

  if _line.action == "won" or _line.action == "dead" then
    table.insert(seq,{_line.action,_line.x,_line.y})
    break
  end

  if  then
    table.insert(seq,{"danger",_line.x,_line.y})
  end
  if _line.action == "pickup_item" then
    table.insert(seq,{_line.content,_line.x,_line.y})
  end 

  if _line.action == "attack_monster" or _line.action == "potential_danger" or _line.action == "safe"then
    table.insert(seq,{_line.action,_line.x,_line.y})
  end  

end




--[[ 

IN:
---

move_to(void,X,Y) 

dead(void,void,void) 
won(void,void,void) 


safe(void,X,Y)
potential_danger(void,X,Y)

attack_monster(void,X,Y)

pickup_item(Item,X,Y)

actual_danger(ActDanger,X,Y) 
item(Item,X,Y)

Item = {rupee,sword,heart}
ActDanger = {monster,vortex,hole}

OUT:
---

teleport
move

rupee
sword
heart

monster
vortex
hole

safe
potential_danger
attack_monster

won
dead 

always with an x y in the end
--]]

