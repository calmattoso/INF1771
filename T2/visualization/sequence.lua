require("search")

seq = {}
path = "../logs/vis.log"


function parse(line)
  local _line = {}
  _,_,_line.action,_line.content,_line.x,_line.y = string.find(line, "([a-z_]+).(%a+).(%d+).(%d+).")
  return _line
end



function insert( action, x, y )
  --http://www.lua.org/pil/13.1.html
  if action == "safe" or action == "actual_danger" or action == "potential_danger" then
    if not exists(seq,action,x,y) then
      table.insert(seq,{["action"] = action,["x"] = tonumber(y),["y"] = tonumber(x)})
    end
  else
    table.insert(seq,{["action"] = action,["x"] = tonumber(y),["y"] = tonumber(x)})
  end
end



_items = {}
for line in io.lines(path) do 
  _line = parse(line)
  -- usar http://lua-users.org/wiki/SwitchStatement ?
  if _line.action == "move_to" then
    if exists(_items,"vortex",_line.x,_line.y) then
      insert("teleport",_line.x,_line.y)
    else  
      --print(string.format("(%d,%d)->(%d,%d)",link_x,link_y,_line.x,_line.y))
      print("carregando...")
      --a_star(_line.x,_line.y) 
      bfs(_line.x,_line.y)
      --insert("move",_line.x,_line.y)
    end
    link_x = _line.x
    link_y = _line.y
  end

  if _line.action == "actual_danger" or _line.action == "item" then
    insert(_line.content,_line.x,_line.y)
    table.insert(_items,{_line.content,_line.x,_line.y})
  end

  if _line.action == "won" or _line.action == "dead" then
    insert(_line.action,_line.x,_line.y)
    break
  end

  if _line.action == "pickup_item" then
    insert(_line.content,_line.x,_line.y)
  end 

  if _line.action == "attack_monster" or _line.action == "potential_danger" or _line.action == "safe"then
    insert(_line.action,_line.x,_line.y)
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

