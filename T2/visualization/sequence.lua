seq = {}
path = "../logs/solution.log"
--file = io.open(path)

for line in io.lines(path) do 
  line:gsub(".",function(c) table.insert(seq,tonumber(c)) end)
end

--file:close(path)



--[[
1 Mover para Frente; 
2 Virar a Direita (rotação de 90°); 
3 Virar a Esquerda (rotação de 90°);
4 Atacar 
5 Pegar Rupia 
6 Pegar Master Sword 
7 entrar no portal
--]]

