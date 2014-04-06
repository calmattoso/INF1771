
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
 link.initial_x = 24
 link.initial_y = 27

loadtiles(t)

maps = {}
gates = {}

maps[1],gates[1] = read_map("mapa0.txt",1)
maps[2],gates[2] = read_map("dun1.txt",2)
maps[3],gates[3] = read_map("dun2.txt",3)
maps[4],gates[4] = read_map("dun3.txt",4)

actual = 1

----link----

 link.x = link.initial_x*t
 link.y = link.initial_y*t
 link.w = 33
 link.h = 33
 link.anim = {}
 link.anim = anims.baixo

    
    ----utils----
    T = 0
    Sn = 1
    Sm = 1
    cost = 0
    step = 0.4
    temp_step = 0.4
 ----tile set batch-----
    item = {1,1,1,1}
  updateTilesetBatch(maps[actual],gates[actual])
    ----sound----
    TEsound.play(song[actual])
    
    
end

function update_view(dt)
   if T>step then 
   step = temp_step
   if Sm<#seq then 
     if seq[Sm][Sn]>0 then 
       move(seq[Sm][Sn])
     end
     if seq[Sm][Sn]==0 then 
       print("entrou")
       action()
       Sm = Sm + 1
       Sn = 1
     end  
     Sn = Sn + 1
   else  
     --finished 
   end
   T = 0
 end
  T = T + dt
end

function love.update(dt)
  link.anim:update(dt)
  update_view(dt)
  TEsound.cleanup()
end

function action()
  if actual == 1 then --mapa
      old_link_x = link.x
      old_link_y = link.y
      for i=1, #gates[actual] do
        if link.x == gates[actual][i].x and link.y == gates[actual][i].y then
            actual = gates[actual][i].dest--change map
            print(actual)
            TEsound.play(song[actual])
            link.x = gates[actual][2].x
            link.y = gates[actual][2].y
        end
      end
      
  else --dungeon
    if link.x == gates[actual][1].x and link.y == gates[actual][1].y then --item
      temp_step = step
      step = 2
      TEsound.play(song_item)
      item[actual] = 0
    else  
      actual = 1
      TEsound.play(song[actual])
      link.x = old_link_x
      link.y = old_link_y
    end
  end
end


function love.keypressed(key, unicode)
  if key == "a" then link.x = link.x - t;link.anim = anims.esq end
  if key == "s" then link.y = link.y + t;link.anim = anims.baixo end
  if key == "d" then link.x = link.x + t;link.anim = anims.dir end
  if key == "w" then link.y = link.y - t;link.anim = anims.cima end
end

function move(way)
  if way == 1 then link.y = link.y - t;link.anim = anims.cima end
  if way == 2 then link.x = link.x + t;link.anim = anims.dir end
  if way == 3 then link.y = link.y + t;link.anim = anims.baixo end
  if way == 4 then link.x = link.x - t;link.anim = anims.esq end
  cost = cost + costs[maps[actual][link.y/t][link.x/t]]
end



function drawlink()
    love.graphics.push()
    love.graphics.scale(t/link.w, t/link.h)
    link.anim:draw(link.x/(t/link.w), link.y/(t/link.h))
    love.graphics.pop()
end


function love.draw()

 love.graphics.draw(tilesetBatch)
 drawlink()
 love.graphics.print("The legend of zelda\n A* to the past",(n+10)*t,5*t) 
 love.graphics.print("cost: "..cost,(n+1)*t,15*t)
 --if item[actual] then 
    --desenha assets_item[actual] em gates[actual][1].x,gates[actual][1].y
 --end
end

function love.quit()
  
end
