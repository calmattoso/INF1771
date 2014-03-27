#ifndef _MAP_H_
#define _MAP_H_

#include <string>
#include <vector>

using namespace std;

typedef pair<int,int> Coord; /* column, line */

class Map {
private:
  /* Negative costs in order to use stl/boost max-heap */
  enum TerrainTypes 
  {
    GRASS    = -10, /* same for light dungeon area */
    SAND     = -20,
    FOREST   = -100,
    MOUNTAIN = -150,
    WATER    = -180,
    INF      = -100000 /* insanely high */
  };

  static const int MAP_LEN = 42;

  /* Added margins to avoid overflow, with a ludicrous cost */
  vector<Map::TerrainTypes> map[MAP_LEN + 2];
  int length;      

  void ParseMap(string );

public:    
    Map(string , int);

    void Display(bool);

    vector< Coord > Solve( Coord , Coord );

};

#endif
