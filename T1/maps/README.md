Input Map File Grammar
======================

<info> \n\n <gates> \n\n <map>

<info>: id n_gates width height 
<gates>: n_gates*<gate>  
  the <gates> semantics change with the id of the map:
  if id == 1 then
    map is overworld, 
    each gate will be where the gate is on the map and to which dungeon the gate is for
  else
    the first gate is the item location and the id of the item
    and the second the position of the gate that will lead to overworld 
<gate>: x y dest_id
<map>: height*<line>
<line>: width*<char>
<char> = {F,G,S,M,W,D,L}

