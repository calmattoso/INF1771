require("assets")

function loadtiles(t)
  tilesetImage = assetsTiles
  tilesetImage:setFilter("nearest", "linear") 
  tileQuads = {}
  for i=1,4 do
    tileQuads[i] = love.graphics.newQuad(0, (i-1) * 30, 30, 30,
      tilesetImage:getWidth(), tilesetImage:getHeight())
  end
  tilesetmap = love.graphics.newSpriteBatch(tilesetImage, n * n)
end
--[[
  1 = grass
  2 = wall
  3 = danger
  4 = fog
--]]
function updatemap(map,fog)
  
  tilesetmap:bind()
  tilesetmap:clear()

  for x=1, #map do
    for y=1, #map[x] do
      if fog[x][y] > 0 then
        tilesetmap:add(tileQuads[fog[x][y]+2], (y-1)*t, (x-1)*t,0,t/30,t/30)
      else
	      tilesetmap:add(tileQuads[map[x][y]], (y-1)*t, (x-1)*t,0,t/30,t/30)--cell
      end
    end
  end
  tilesetmap:unbind()
end
