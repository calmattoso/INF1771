#include <iostream>
#include <cmath>
#include <set>
#include "map.h"

using namespace std;

/* Public */

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
        default            : code = '-';
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

State Map::Solve(Coord start, Coord goal)
{
  set< State > pq ; /* Priority queue */
  vector<bool> visited( this->length * this->length + 1 , false ); /* Visited nodes are marked; w/ margin */
  State solution;

  State initial = make_pair( make_pair(0,0) , Path( 1, start ) );
  pq.insert(initial);

  while( !pq.empty() )
  {
    /* Get the current path with least cost, and
        remove it from our "heap" */
    State top = *(pq.begin());
    pq.erase( pq.begin() );
    
    /* Get the last visited node of the candidate path */
    Coord last = *(top.second.rbegin());
    
    /* Identify possible new expansions*/
    vector< Coord > candidates = ExpandFrontier( last );

    //cout << "(" << last.first << "," << last.second << ") [" << top.first << "]" << endl;

    for( int i = 0, len = candidates.size(); i < len ; i++)
    {
      int col  = candidates[i].first,
          line = candidates[i].second,
          idx  = ( line - 1 ) * (this->length) + col ;

      /* Adding to the heap paths which do not take us back
          to a previously seen node; if the goal state is reached,
          exit the loop and return it */ 
      if( !visited[ idx ] )
      {
        visited[idx] = true;

        /* totalCost is an aggregate of: 
            sum of hops cost + 
            sum of manhattan distances
           stepsCost is an aggregate of hops cost */
        int totalCost = top.first.first + map[line][col] + 
                ManhattanDistance( candidates[i] , goal ) ,
            stepsCost = top.first.second + map[line][col];

       // cout << "\t@(" << col << "," << line << ") cost: " << newCost << endl;

        Path newPath(top.second.begin(), top.second.end());
        newPath.push_back( candidates[i] );

        State next( make_pair( totalCost , stepsCost ), newPath );
        pq.insert( next );

        /* check if we reached the goal */
        if( candidates[i] == goal )
        {
          solution = next;
          break;
        }
      }
    }
  } 

  return solution;
}


/* Private Helpers */

bool Map::fncomp (State lhs, State rhs) 
{ 
  return lhs.first < rhs.first ; 
}

int Map::ManhattanDistance( Coord from, Coord to )
{
  return abs( to.first - from.first ) + abs( to.second - from.second );
}

vector< Coord > Map::ExpandFrontier ( Coord pos ) 
{
  /* Right, Left, Down, Up */
  Coord diffs[4] = {
    Coord( 1 , 0 ) , Coord( -1 ,  0 ) ,
    Coord( 0 , 1 ) , Coord(  0 , -1 )
  };

  vector< Coord > valid_neighs;

  /* Get each possible neighbor, check if it's valid, return final list */
  for(int i = 0; i < 4; i++)
  {
    Coord neighbor = make_pair( 
      pos.first  + diffs[i].first,
      pos.second + diffs[i].second 
    );

    if( map[ neighbor.second ][ neighbor.first ] != Map::INF )
      valid_neighs.push_back( neighbor ) ;
  }

  return valid_neighs;
}



