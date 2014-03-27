#ifndef _MAP_H_
#define _MAP_H_

#include <string>
#include <vector>

using namespace std;

/* column (x), line (y) */
typedef pair<int,int> Coord; 

/* Sequence of nodes */
typedef vector< Coord > Path;

/* total cost ( w/ h(node) ), movements cost */
typedef pair<int, int> Cost;

/* cost, path */
typedef pair< Cost , Path > State;

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
  int length;      

  void ParseMap( string );

  int ManhattanDistance( Coord , Coord );

  vector< Coord > ExpandFrontier ( Coord ) ;

  static bool fncomp ( State , State ) ;

public:    
    Map(string , int);

    void Display(bool);

    State Solve( Coord , Coord );

};

#endif
