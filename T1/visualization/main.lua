
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

maps = {}
gates = {}
maps_basedir = "../maps/"
maps[1],gates[1] = read_map(maps_basedir.."mapa0.txt",1)
maps[2],gates[2] = read_map(maps_basedir.."dun1.txt",2)
maps[3],gates[3] = read_map(maps_basedir.."dun2.txt",3)
maps[4],gates[4] = read_map(maps_basedir.."dun3.txt",4)

link.initial_x = gates[1][1].x  
link.initial_y = gates[1][1].y 
table.remove(gates[1],1)

lost_woods_x = gates[1][#gates[1]].x 
lost_woods_y = gates[1][#gates[1]].y 
table.remove(gates[1],#gates[1])
costs[8] = costs[maps[1][link.initial_y][link.initial_x]]
maps[1][link.initial_y+1][link.initial_x+1] = 8 --link's house

costs[7] = costs[maps[1][lost_woods_x][lost_woods_y+2]]
maps[1][lost_woods_x][lost_woods_y+2] = 7 --lost woods entrance

actual = 1

loadtiles(t)

----link----

 link.x = (link.initial_x)*t
 link.y = (link.initial_y)*t
 link.w = 33
 link.h = 33
 link.anim = {}
 link.anim = anims.baixo

    
    ----utils----
    T = 0
    Sn = 1
    Sm = 1
    cost = 0
    step = 0.2
    temp_step = step
    started = false
    press_text = false
 ----tile set batch-----
    item = {}
    for i=2,4 do
	item[i] = {}
	item[i].value = 0
        item[i].x = gates[i][1].x
	item[i].y = gates[i][1].y
    end
	item[2].text = "wisdom"
	item[3].text = "courage"
	item[4].text = "strength"

item[5] = 0
  updateTilesetBatch(maps[actual],gates[actual])
    ----sound----
    

TEsound.play(song[actual],"world")
    
end

function update_view(dt)
   if T>step then 
   step = temp_step
   if Sm<#seq+1 then 
     if seq[Sm][Sn]==0 then 
       --print("entrou")
       action()
       Sm = Sm + 1
       Sn = 1
       --[[if Sm == #seq+1 then 
		step = 100000000
		temp_step = 10000000
		Sm=1--]]
	end 
     if Sm ~= #seq+1 then 
     if seq[Sm][Sn]>0 then 
     	  move(seq[Sm][Sn])
     end
    end
     Sn = Sn + 1
   else  
     step = 1000000000
     temp_step = 10000000000
     item[5] = cost
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

function action()
 if actual ~= 1 then --dungeon
     if link.x/t == gates[actual][1].x and link.y/t == gates[actual][1].y then --item
      temp_step = step
      step = 2
      TEsound.play(song_item)
      item[actual].value = cost
      link.x = link.x
     else  
      TEsound.stop("dun")
      actual = 1
      TEsound.play(song[1],"world")
      updateTilesetBatch(maps[actual],gates[actual])
      link.x = old_link_x
      link.y = old_link_y
    end
  else --mapa
       old_link_x = link.x
      old_link_y = link.y
      for i=1, #gates[actual] do
	--print(link.x/t,gates[actual][i].x)
	--print(link.y/t,gates[actual][i].y)
        if link.x/t == gates[actual][i].x and link.y/t == gates[actual][i].y then
            actual = 1 + gates[actual][i].dest--change map
  	    updateTilesetBatch(maps[actual],gates[actual])
	    print("actual:"..actual)
            TEsound.stop("world")
	    TEsound.playLooping(song[actual],"dun")
            link.x = t*(gates[actual][2].x)
            link.y = t*(gates[actual][2].y)
            break
	end
      end
  end	
end

--[[
function love.keypressed(key, unicode)
  if key == "a" then link.x = link.x - t;link.anim = anims.esq end
  if key == "s" then link.y = link.y + t;link.anim = anims.baixo end
  if key == "d" then link.x = link.x + t;link.anim = anims.dir end
  if key == "w" then link.y = link.y - t;link.anim = anims.cima end
end
--]]
function move(way)
  if way == 1 then link.y = link.y - t;link.anim = anims.cima end
  if way == 2 then link.x = link.x + t;link.anim = anims.dir end
  if way == 3 then link.y = link.y + t;link.anim = anims.baixo end
  if way == 4 then link.x = link.x - t;link.anim = anims.esq end
  cost = cost + costs[maps[actual][link.y/t+1][link.x/t+1]]
--  print(link.x/t,link.y/t,maps[actual][link.y/t+1][link.x/t+1])
end

function love.keypressed(key)
   if not started then
      started = true
   end
end

function drawlink()
    love.graphics.push()
    love.graphics.scale(t/link.w, t/link.h)
    link.anim:draw(link.x/(t/link.w), link.y/(t/link.h))
    love.graphics.pop()
end


function love.draw()
if started then
 love.graphics.draw(tilesetBatch)
 drawlink()
 --love.graphics.print("The legend of zelda\n A* to the past",(n+10)*t,5*t) 
 love.graphics.draw(logo,(n+1)*t,5*t,0,0.01*t,0.01*t)
 love.graphics.print("cost: "..cost,(n+1)*t,15*t)
	
 for i=2,4 do  
	if item[i].value>0 then 
		love.graphics.print("got jewel of "..item[i].text.." with this effort: "..item[i].value,(n+1)*t,(15+i)*t) 
		if i==actual then love.graphics.draw(chest_o,item[i].x*t,item[i].y*t) end--scale
	elseif i ==actual then love.graphics.draw(chest_c,item[i].x*t,item[i].y*t)
	end 
 end
 if item[5] > 0 then love.graphics.print("Thank you for watching!\nFinal cost: "..item[5],(n+1)*t,(20)*t); end
else
	love.graphics.draw(logo,0,0,0,window[1]/logo:getWidth(),window[2]/logo:getHeight())
	if press_text then love.graphics.print("Press any button to start", window[1]/2,7*window[2]/8, 0, 3, 3) end

end
end
function love.quit()
  
end
