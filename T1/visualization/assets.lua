color = {}
color[1] = {255,0,0}
color[2] = {0,255,0}
color[3] = {0,0,255}
color[4] = {255,255,255}
color[5] = {0,0,0}
color[6] = {100,100,100} 
color[7] = {0,255,255}
---link------
basedir = "/assets/"

local lb  = love.graphics.newImage( basedir.."lb.png")
local lc  = love.graphics.newImage( basedir.."lc.png")
local le  = love.graphics.newImage( basedir.."le.png")
local ld  = love.graphics.newImage( basedir.."ld.png")

anims = {}
    anims.baixo = newAnimation(lb, 32.5, 35, 0.1, 6)
    anims.cima = newAnimation(lc, 32.5, 35, 0.1, 6)
    anims.esq = newAnimation(le, 33, 31, 0.1, 6)
    anims.dir = newAnimation(ld, 34, 32, 0.1, 6)
    
assetsTiles = love.graphics.newImage( basedir.."tiles2.png" )

song = {}
song[1] = basedir.."song.mp3"
song[2] = basedir.."dun1.mp3"
song[3] = basedir.."dun1.mp3"
song[4] = basedir.."dun1.mp3"
song_item = basedir.."item.mp3"
