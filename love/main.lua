
require("AnAL")
--TEsound

function love.load()

 lx = 300 
 ly = 300
 w = 300
 h = 300
 -- sendtoC (map1,map2,map3,map4)
 t = 30
 map = {}
 for i = 1, w/t do
  map[i] = {}
  for j = 1, h/t do  
   map[i][j] = 0 --read from file
  end
 end 
--imagem
    local lb  = love.graphics.newImage("lb.png")
    local lc  = love.graphics.newImage("lc.png")
    local le  = love.graphics.newImage("le.png")
    local ld  = love.graphics.newImage("ld.png")
    anims = {}
    anims["baixo"] = newAnimation(lb, 32.5, 35, 0.1, 6)
    anims["cima"] = newAnimation(lc, 32.5, 35, 0.1, 6)
    anims["esq"] = newAnimation(le, 34, 31, 0.1, 6)
    anims["dir"] = newAnimation(ld, 34, 31, 0.1, 6)
    anim = {}
    anim = anims["baixo"]
  --music play
end

function love.update(dt)
  anim:update(dt)   
 -- sendtoC(lx,ly,ox,oy,map) --send link x,y and actual objective x,y and which map to use
			 --send the 4 maps at love.load
 --getfromC(nx,ny) --link's new x,y.
end


function love.keypressed(key, unicode)
  if key == "a" then lx = lx - t;anim = anims["esq"] end
  if key == "s" then ly = ly + t;anim = anims["baixo"] end
  if key == "d" then lx = lx + t;anim = anims["dir"] end
  if key == "w" then ly = ly - t; anim = anims["cima"] end
end
function love.draw()
  anim:draw(lx, ly) --link


 --otimizar com https://love2d.org/wiki/SpriteBatch

  for i = 1, w/t do
    for j = 1, h/t do  
      love.graphics.rectangle("line",i*t,j*t,t,t)
      love.graphics.print(map[i][j],i*t+t/2,j*t+t/2)
    end
  end 
  
  love.graphics.print("IA",400,300)
end

function love.quit()
  
end
