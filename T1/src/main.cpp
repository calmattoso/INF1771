#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include "map.h"
#include "utils.h"

using namespace std;

int main(){
  string strMain     = ReadFile("../maps/mapa_zelda1.txt"  ),
         strDungeon1 = ReadFile("../maps/mapa_dungeon1.txt"),
         strDungeon2 = ReadFile("../maps/mapa_dungeon2.txt"),
         strDungeon3 = ReadFile("../maps/mapa_dungeon3.txt") ;
   
  Map dungeon1( strDungeon1 , 28 );
  dungeon1.Display(true);
  vector< Coord > solution = dungeon1.Solve(make_pair(0,0), make_pair(1,1));

  LogSolution("../logs/dg1.log", "Dungeon 1", solution);


  return 0;
}



