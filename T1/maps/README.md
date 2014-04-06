## Map Definition File Format

Input files will respect the following format:

```html
<info>
<gates>
<map>
```

The above tags respect the following grammar:


- `<info>`: **id** &nbsp; **n_gates** &nbsp; **width** &nbsp; **height**
  - **id**: id of the map
  - **n_gates**: number of gates on the map
  - **width**: number of columns
  - **height**: number of lines
- `<gates>`: **n_gates** * `<gate>`  
  - the `<gates>` semantics change with the **id** of the map:    
    if **id** == 1 then    
      &nbsp;&nbsp; map is overworld,    
      &nbsp;&nbsp; the first gate is the start position
      &nbsp;&nbsp; each middle gate is the position of the gate on the map followed by the destination dungeon id
      &nbsp;&nbsp; the last gate is the final position    
    else    
      &nbsp;&nbsp; the first gate is the item location followed by the id of the item    
      &nbsp;&nbsp; the second gate is the position of the gate that will take to overworld 
    obs: **position** is defined as: **column** &nbsp; **line**
    
- `<gate>`: **x** &nbsp; **y** &nbsp; **dest_id**
- `<map>` : **height** * `<line>`
- `<line>`: **width** * `<char>`
- `<char>`: { F , G , S , M , W , D , L }

