function rev(s)
  local r = {}
  for i=#s-1,1,-1 do
     r[#r+1] = s[i]
  end
  r[#r+1] = 0
  return r
end

seq = {}
path = "solution.log"
--file = io.open(path)

for line in io.lines(path) do 
  seq[#seq+1] = {}
  line:gsub(".",function(c) table.insert(seq[#seq],tonumber(c)) end)
end

--file:close(path)



--[[
-- (1,2,3,4) -> (esq,baixo,dir,cima)
  path = "mapa.log"
  file = io.open(path)
  seq[1] = file:read("*line")
  seq[4] = file:read("*line")
  seq[7] = file:read("*line")
  seq[10] = file:read("*line")
  file:close(path)

  for i=1,3 do
    path = "dg"..i..".log"
    file = io.open(path)
    cost = file:read("*num") 
    seq[i-1+2*i] = file:read("*all")
    seq[i+2*i] = rev(seq[i])
    file:close(path)
  end
--]]

