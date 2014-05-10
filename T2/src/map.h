#ifndef _MAP_H_
#define _MAP_H_

#include <string>
#include <vector>
#include "map_config.h"

using namespace std;

class Map {
public:
  enum Direction
  {
    POS_UP    ,
    POS_RIGHT ,
    POS_LEFT  ,
    POS_DOWN  
  };

private:
  /* Negative costs in order to use stl/boost max-heap */
  enum TerrainTypes 
  {
    GRASS   = 1,
    FOREST  = 2,
    INF = 10000000
  };

  static const int MAP_LEN = 42;

private:
  /* Added margins to avoid overflow, with a ludicrous cost */
  vector<Map::TerrainTypes> map[MAP_LEN + 2];

  /* square matrix, width == height */
  int length;  

private:   
/* Helper Methods */
  void ParseMap( string );

  int ManhattanDistance( Coord , Coord );

  inline bool IsValidCoord( Coord pos );

  vector< Coord > ExpandFrontier ( Coord ) ;

public:
/* Class API */
  
  static Map::Direction relativeDirection( Coord , Coord );

  static bool isAdjacent( Coord , Coord );

public:    
/* Instance API */
  /* all gates/entities on the map */
  vector< Gate > gates;
  
  Map( MapConfig , bool );

  void Display( bool showCost );

  State Solve( Coord , Coord );

};

#endif
