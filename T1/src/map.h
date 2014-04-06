#ifndef _MAP_H_
#define _MAP_H_

#include <string>
#include <vector>
#include "map_config.h"

using namespace std;

class Map {
private:
  /* Negative costs in order to use stl/boost max-heap */
  enum TerrainTypes 
  {
    GRASS    = 10, /* same for light dungeon area */
    SAND     = 20,
    FOREST   = 100,
    MOUNTAIN = 150,
    WATER    = 180,
    INF      = 1000000 /* insanely high */
  };

  static const int MAP_LEN = 42;

  /* Added margins to avoid overflow, with a ludicrous cost */
  vector<Map::TerrainTypes> map[MAP_LEN + 2];

  /* square matrix, width == height */
  int length;      

  void ParseMap( string );

  int ManhattanDistance( Coord , Coord );

  inline bool IsValidCoord( Coord pos );

  vector< Coord > ExpandFrontier ( Coord ) ;

public:    
  bool isDungeon;

  /* all gates/entities on the map */
  vector< Gate > gates;
  
  Map( MapConfig , bool );

  void Display( bool showCost );

  State Solve( Coord , Coord );

};

#endif
