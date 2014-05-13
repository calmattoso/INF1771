require ("prolog_aux")

io.output("src/prolog/problem.pl")

io.write(header)

file = io.open("data/map_definition.txt")
text = file:read("*all")
io.write(text)

io.write(footer)