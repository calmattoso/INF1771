
require("AnAL")
require("TEsound")
require("tileset")
require("assets")
require("sequence")
--require("drawmap")
require("readmap")
require("terrain")


function love.load()

love.window.setFullscreen(true, "desktop")
modes = love.window.getFullscreenModes()
print("available:")
for i=1,#modes do print( modes[i].height,modes[i].width )end

window = {love.window.getMode()}
print("\nused: ", window[1],window[2]) 


 n = 42 -- number of cells
 t = math.floor(window[2]/n) --size of a cell
 w = n*t -- total width of the map
 h = n*t -- total height of the map
 link = {}

map = {}
fog = {}
items = {}
dangers = {}

map = read_map("../data/map.txt")
for x=1,#map do
  fog[x] = {}
  for y=1,#map[1] do
    if map[x][y] == 1 then 
      fog[x][y] = 2 
    else 
      fog[x][y] = 0 
    end 
  end
end

-- fog = 0 => safe
-- fog = 1 => danger
-- fog = 2 => unknown


link.initial_x = 22
link.initial_y = 39


loadtiles(t)

----link----

 link.x = (link.initial_x)*t
 link.y = (link.initial_y)*t
 link.w = 33
 link.h = 33
 link.anim = {}
 link.anim = anims.baixo
 way = 3
    
    ----utils----
    T = 0
    Sn = 1
    Sm = 1
    cost = 0
    energy = 100
    step = 0.01
    temp_step = step
    started = false
    press_text = false
    win = false
 ----tile set batch-----
    
updatemap(map,fog)
    ----sound----
    

TEsound.play(song,"")

end

function update_view(dt)
  if T>step then 
   step = temp_step
   if Sm <= #seq then 
       action(seq[Sm])
       Sm = Sm + 1
	end 
   T = 0
 end
  T = T + dt
end

function love.update(dt)
 
  if started then
	  update_view(dt)
  	link.anim:update(dt)
  else
	if T>step then 
		press_text = not press_text 
		T = 0
	end 
	T = T + dt
  end
TEsound.cleanup()
end

function get_way(y,x)
  lx = link.x/t
  ly = link.y/t
  print(x,y,lx,ly)
  if x == lx then
    if y > ly then 
      return 3
    else 
      return 1
    end
  end
  if y == ly then
    if x > lx then 
      return 2
    else 
      return 4
    end
  end
end

function effect(item)
  --temp_step = step
  --step = 2
  --TEsound.play(song_item)
  if item == "rupee" then
    cost = cost + 10
  elseif item == "heart" then
    cost = cost - 10
    energy = energy + 50
  elseif item == "sword" then
    cost = cost - 100
  end
end

function exists( table, action,x,y )
  for a,e in pairs(table) do
    if e.action == action and e.x == x and e.y == y then
      return a
    end
  end
  return false
end

function action(todo)
  if todo.action == "move" then   
    new_way = get_way(todo.x,todo.y)
    if way ~= new_way then --TODO if turn 180 decrease 2
      way = new_way
      cost = cost - 1
    end
    move(way)
    cost = cost - 1
  end
  if todo.action == "teleport" then   
   link.x = todo.x
   link.y = todo.y
  end
  if todo.action == "monster" or todo.action == "vortex" or todo.action == "hole" then
    table.insert(dangers,todo)
  end 
  if todo.action == "rupee" or todo.action == "sword" or todo.action == "heart" then
    pos = exists(items,todo.action,todo.x,todo.y)
    if pos then 
      effect(todo.action)
      table.remove(items,pos)
    else
      table.insert(items,todo)
    end
  end
  if todo.action == "safe" then 
    fog[todo.x][todo.y] = 0
    updatemap(map,fog)
  end
  if todo.action == "potential_danger" then
    fog[todo.x][todo.y] = 1
    updatemap(map,fog)
  end
  if todo.action == "won" then
    win = true
  end
  if todo.action == "dead" then
    --game over
  end

  if todo.action == "attack_monster" then 
    --animacao de atacar(get_way(x,y))
    energy = energy - 10
    cost = cost - 5
    pos = exists(dangers,"monster",todo.x,todo.y)
    table.remove(dangers,pos)
  end
end
--[[      temp_step = step
      step = 2
      TEsound.play(song_item)
     --]]



function move(way)
  print("link",way)
  if way == 1 then link.y = link.y - t;link.anim = anims.cima end
  if way == 2 then link.x = link.x + t;link.anim = anims.dir end
  if way == 3 then link.y = link.y + t;link.anim = anims.baixo end
  if way == 4 then link.x = link.x - t;link.anim = anims.esq end
end

function love.keypressed(key)
   if not started then
      started = true
   end
end

function draw_link()
    love.graphics.push()
    love.graphics.scale(t/link.w, t/link.h)
    link.anim:draw(link.x/(t/link.w), link.y/(t/link.h))
    love.graphics.pop()
end

danger_icon = {["monster"] = monster,["hole"] = hole,["vortex"] = vortex}

function draw_dangers()
  for _,danger in pairs(dangers) do
    love.graphics.draw(danger_icon[danger.action],(danger.x)*t,danger.y*t,0,t/30,t/30)
  end
end

item_icon = { ["rupee"] = rupee, ["heart"] = heart, ["sword"] = sword }
function draw_items()
  for _,item in pairs(items) do
    love.graphics.draw(item_icon[item.action],item.x*t,item.y*t,0,t/30,t/30)
  end
end

function love.draw()
  if started then
   love.graphics.draw(tilesetmap)
   draw_dangers()
   draw_items()
   draw_link()
   love.graphics.draw(logo,(n+2)*t,5*t,0,0.01*t,0.01*t)
   love.graphics.print("cost: "..cost,(n+2)*t,15*t)
   love.graphics.print("energy: "..energy,100+(n+2)*t,15*t)


    if win == true then 
      love.graphics.print("Thank you for watching!\nFinal cost: "..cost,(n+1)*t,(20)*t); 
    end
  else
    	love.graphics.draw(logo,0,0,0,window[1]/logo:getWidth(),window[2]/logo:getHeight())
    	if press_text then 
        love.graphics.print("Press any button to start", window[1]/2,7*window[2]/8, 0, 3, 3) 
      end
  end
end

function love.quit()
  
end
