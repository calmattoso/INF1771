require("readmap")
local _map = read_map("../data/map.txt") 
link_x = 22
link_y = 39


function exists( table, action,x,y )
  for _,e in pairs(table) do
    if e.action == action and e.x == x and e.y == y then
      return true
    end
  end
  return false
end

function dist(a,b)
  local x1,y1 = unflat(a)
  local x2,y2 = unflat(b)
  sqx = (x1-x2) * (x1-x2)
  sqy = (y1-y2) * (y1-y2)
  return math.sqrt( sqx + sqy )
end

function flat(x,y)
  return (x)*42 + (y)
end

function unflat(v)
  return math.floor(v/42),v%42
end

function validate_create(a,b)
  if a<42 and a>0 and b>0 and b<42 then
    if _map[b][a] == 1 then 
      if not exists(_items,"hole",a,b) and not exists(_items,"monster",a,b) then
        return true
      end
    else --print("parede")
    end
  else
  --print("bound")
  end
  return false
end

function neighbors_create(n)
  local n1,n2 = unflat(n)
  local r = {}
  if validate_create(n1,n2+1) then table.insert(r,flat(n1,n2+1)) end
  if validate_create(n1,n2-1) then table.insert(r,flat(n1,n2-1)) end
  if validate_create(n1+1,n2) then table.insert(r,flat(n1+1,n2)) end
  if validate_create(n1-1,n2) then table.insert(r,flat(n1-1,n2)) end
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

function search_best(table, f )
  m = math.huge
  for k,v in pairs(table) do
    if f[v] < m then 
      mk = k
      m = f[v]
    end
  end
  return mk
end

function a_star(dst_x,dst_y)
  local pos = flat(link_x,link_y)
  local goal = flat(dst_x,dst_y)
  local closedset = {}
  local openset = {}
  local came_from = {}
  local g = {}
  local f = {}
  table.insert(openset,pos)
  g[pos] = 0
  f[pos] = g[pos] + dist(pos,goal)

  while #openset ~= 0 do
    current = openset[search_best(openset,f)]
    --if openset[current] then print(openset[current]) end
    if current == goal then
      reconstruct_path(came_from,goal)
      --for k,v in pairs(came_from) do print(unflat(v))  end
      return
    end
    table.insert(closedset,current)
    table.remove(openset,search(openset,current))
    neighbors = neighbors_create(current)
    for _,neighbor in pairs(neighbors) do
      if not exists_flat(closedset,neighbor) then
        t_g = g[current] + dist(current,neighbor)
          if not exists_flat(openset,neighbor) or t_g < g[neighbor] then
            came_from[current] = neighbor
            g[neighbor] = t_g
            f[neighbor] = g[neighbor] + dist(neighbor, goal)
            if not exists_flat(openset,neighbor) then
              table.insert(openset,neighbor)
            end
          end
      end
    end
  end
end
   
function reconstruct_path(came_from, current_node)
    --if exists_flat(came_from,current_node) then
    if came_from[current_node] then
      reconstruct_path(came_from, came_from[current_node])
        insert("move",unflat(current_node))
      print("move",unflat(current_node))
    end
end

function bfs(dst_x,dst_y)
  local pos = flat(link_x,link_y)
  local goal = flat(dst_x,dst_y)
  --print(pos,goal)
  --print(validate_create(unflat(goal)))
  local closedset = {}
  local openset = {}
  local came_from = {}
  table.insert(openset,pos)
  while #openset ~= 0 do
    --print(current)
    current = openset[1]
    --if openset[current] then print(openset[current]) end
    if current == goal then
      --print("goal",current)
      reconstruct_path(came_from,goal)
      --for k,v in pairs(came_from) do print(unflat(v))  end
      return
    end
    table.insert(closedset,current)
    table.remove(openset,1)
    neighbors = neighbors_create(current)
    for _,neighbor in pairs(neighbors) do
      if not exists_flat(closedset,neighbor) then
          if not exists_flat(openset,neighbor) then
            came_from[neighbor] = current
            table.insert(openset,neighbor)
          end
      end
    end
  end
end
