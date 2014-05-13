require("readmap")

seq = {}
path = "../logs/vis.log"

--for a_star:
map = read_map("../data/map.txt") 
link_x = 22
link_y = 39

function parse(line)
  local _line = {}
  _,_,_line.action,_line.content,_line.x,_line.y = string.find(line, "([a-z_]+).(%a+).(%d+).(%d+).")
  return _line
end

function exists( table, action,x,y )
  for _,e in pairs(table) do
    if e.action == action and e.x == x and e.y == y then
      return true
    end
  end
  return false
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

function dist(a,b)
  local x1,y1 = unflat(a)
  local x2,y2 = unflat(b)
  sqx = (x1-x2) * (x1-x2)
  sqy = (y1-y2) * (y1-y2)
  return math.sqrt( sqx + sqy )
end

function flat(x,y)
  return (x-1)*42 + (y-1)
end

function unflat(v)
  return math.floor(v/42),v%42
end

function neighbors_create(n)
  local n1,n2 = unflat(n)
  local r = {}
  table.insert(r,flat(n1,n2+1))
  table.insert(r,flat(n1,n2-1))
  table.insert(r,flat(n1+1,n2))
  table.insert(r,flat(n1-1,n2))
  return r
end

function exists_flat(table,n)
  for k,v in pairs(table) do
    if v == n then return true end
  end
  return false
end

function search(table,n )
  for k,v in pairs(table) do
    if v == n then return k end
  end
end

function a_star(dst_x,dst_y)
  local pos = flat(link_x,link_y)
  local goal = flat(dst_x,dst_y)
  closedset = {}
  openset = {}
  table.insert(openset,pos)
  came_from = {}
  g = {}
  g[pos] = 0
  f = {}
  f[pos] = g[pos] + dist(pos,goal)
  while #openset ~= 0 do
    table.sort( openset, function(a,b) return f[a]<f[b] end )
    if openset[1] == goal then
      reconstruct_path(came_from,goal)
      break
    end
    current = openset[1]
    table.insert(closedset,current)
    table.remove(openset,search(openset,current))
    neighbors = neighbors_create(current)
    for _,neighbor in pairs(neighbors) do
            if not exists_flat(closedset,neighbor) then
              t_g = g[current] + dist(current,neighbor)
              if not exists_flat(openset,neighbor) or t_g < g[neighbor] then
                  came_from[neighbor] = current
                  g[neighbor] = t_g
                  f[neighbor] = g[neighbor] + dist(neighbor, goal)
                  if not exists_flat(openset,neighbor) then
                      table.insert(openset,neighbor)
                  end
              end
            end
    end
  end
  return false
end
   
function reconstruct_path(came_from, current_node)
    if exists_flat(came_from,current_node) then
        reconstruct_path(came_from, came_from[current_node])
        --return (p + current_node)
    else
        insert("move",unflat(current_node))
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
      a_star(_line.x,_line.y) 
    end
  end

  if _line.action == "actual_danger" or _line.action == "item" then
    insert(_line.content,_line.x,_line.y)
    if _line.content == "vortex" then
      table.insert(_items,{"vortex",_line.x,_line.y})
    end
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

