#include <iostream>
#include "map.h"

using namespace std;

Map::Map(string mapData, int length)
{
  int totalLen = length + 2;

  for (int i = 0; i < totalLen; ++i)
  {
    map[i].resize(totalLen, Map::GRASS);

    for(int j = 0; j < totalLen; j++)
    { 
      /* set a high cost for the margins,
         to disincentivize A* to ever explore 
         a border */
      if( ( i == 0 || i == length + 1) ||
          ( j == 0 || j == length + 1) )
      {
        map[i][j] = Map::INF;
      }
    }
  }

  this->length = length;

  /* set the cost for each position,
     based on the input string */
  ParseMap(mapData);
}

void Map::ParseMap(string mapData)
{
  int line, col, mapLen = this->length;

  for(int i = 0, len = mapData.size(); i < len; i++ )
  {
    line = ( i / mapLen ) + 1;
    col  = ( i % mapLen ) + 1;

    switch( mapData[i] )
    {
      case 'L': /* has same cost as grass */
      case 'G': map[line][col] = Map::GRASS;    break;
      case 'S': map[line][col] = Map::SAND;     break;
      case 'F': map[line][col] = Map::FOREST;   break;
      case 'M': map[line][col] = Map::MOUNTAIN; break;
      case 'W': map[line][col] = Map::WATER;    break;
      default : map[line][col] = Map::INF;      break; /* set for dark dungeon area */
    }
  }
}

void Map::Display(bool isDungeon)
{
  for(int i = 1; i <= this->length; i++ )
  {
    for(int j = 1; j <= this->length; j++ )
    {
      char code = '-';
      switch( map[i][j] )
      {
        case Map::GRASS    : code = 'G'; break;
        case Map::SAND     : code = 'S'; break;
        case Map::FOREST   : code = 'F'; break;
        case Map::MOUNTAIN : code = 'M'; break;
        case Map::WATER    : code = 'W'; break;
      }

      if(isDungeon && code == 'G')      
        code = 'L';
      else if( isDungeon )
        code = 'D';

      cout << code;
    }
    cout << endl;
  }
}

vector< Coord > Map::Solve(Coord start, Coord goal)
{
  vector< Coord > path;

  path.push_back( make_pair(1,1) );
  path.push_back( make_pair(1,2) );
  path.push_back( make_pair(1,3) );
  path.push_back( make_pair(1,4) );
  path.push_back( make_pair(1,5) );

  return path;
}


