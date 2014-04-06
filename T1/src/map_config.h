#ifndef _MAPCONFIG_H_
#define _MAPCONFIG_H_

#include <utility>
#include <vector>
#include <string>

using namespace std;

/* Useful data types definitions */
/* column (x), line (y) */
typedef pair<int,int> Coord; 

/* gate position (x, y) , ( dest map id | entity id ) */
typedef pair< Coord , int > Gate;

/* Sequence of nodes */
typedef vector< Coord > Path;

/* total cost ( w/ h(node) ), movements cost */
typedef pair<int, int> Cost;

/* cost, path */
typedef pair< Cost , Path > State;


class MapConfig 
{
public:
  int id, width, height;

  /* All gates on the map */
  vector< Gate > gates;

  /* String representation of input map file */
  string str; 
};

#endif