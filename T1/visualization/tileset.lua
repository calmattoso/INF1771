require("assets")

function loadtiles(t)
  tilesetImage = assetsTiles
  tilesetImage:setFilter("nearest", "linear") 
  tileQuads = {}
  for i=1,10 do
    tileQuads[i] = love.graphics.newQuad(0, (i-1) * 30, 30, 30,
      tilesetImage:getWidth(), tilesetImage:getHeight())
  end
  tilesetBatch = love.graphics.newSpriteBatch(tilesetImage, n * n)
end

function updateTilesetBatch(_map,_gate)
  
  tilesetBatch:bind()
  tilesetBatch:clear()


  for x=1, #_map do
    for y=1, #_map[x] do
	    if _map[x][y] ~= nil then
	      tilesetBatch:add(tileQuads[_map[x][y]], (y-1)*t, (x-1)*t,0,t/30,t/30)
      end
    end
  end

  for i=1,#_gate do
    tilesetBatch:set(n*(_gate[i].y) + (_gate[i].x),tileQuads[6], (_gate[i].x)*t, (_gate[i].y)*t,0,t/30,t/30)
  end  

  tilesetBatch:unbind()
end

