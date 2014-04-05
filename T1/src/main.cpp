#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include "map.h"
#include "utils.h"

using namespace std;

int main(){
  string strMain     = Utils::ReadFile("../maps/mapa0.txt"),
         strDungeon1 = Utils::ReadFile("../maps/dun1.txt"),
         strDungeon2 = Utils::ReadFile("../maps/dun2.txt"),
         strDungeon3 = Utils::ReadFile("../maps/dun3.txt") ;
   
  Map overworld( strMain , 42 ),
      dungeon1 ( strDungeon1 , 28 ),
      dungeon2 ( strDungeon2 , 28 ),
      dungeon3 ( strDungeon3 , 28 );
      
  dungeon1.Display(true);

  State solutionDg1 = dungeon1.Solve( make_pair( 15 , 27 ) , make_pair( 14 , 4 ) ),
        solutionDg2 = dungeon2.Solve( make_pair( 14 , 26 ) , make_pair( 14 , 3 ) ),
        solutionDg3 = dungeon3.Solve( make_pair( 15 , 26 ) , make_pair( 16 , 20 ) );


  stringstream header;
  header << solutionDg1.first.second;
  Utils::LogSolution("../logs/dg1.log", header.str(), solutionDg1.second);

  header.str("");
  header << solutionDg2.first.second;
  Utils::LogSolution("../logs/dg2.log", header.str(), solutionDg2.second);

  header.str("");
  header << solutionDg3.first.second;
  Utils::LogSolution("../logs/dg3.log", header.str(), solutionDg3.second);


  return 0;
}



