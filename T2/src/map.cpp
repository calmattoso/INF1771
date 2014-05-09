#include <iostream>
#include <cmath>
#include <queue>

#include "map_config.h"
#include "map.h"

using namespace std;

/* Public */

class SetCompare
{
public:
  
  bool operator() (const State& lhs, const State& rhs) const
  {
    /* first compare total costs */
    if( lhs.first.first != rhs.first.first )
      return lhs.first.first > rhs.first.first;
  }
};

/* Instance API */

Map::Map( MapConfig mapData , 
          bool isDungeon )
{
  int totalLen = mapData.width + 2;

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

  this->length = mapData.width;

  /* copy the gates */
  this->gates.assign(mapData.gates.begin() , mapData.gates.end() ) ;

  /* set the cost for each position,
     based on the input string */
  ParseMap(mapData.str);

  #ifdef DEBUG
    cout << "New map[" << this->length << "] created..." << endl;
    this->Display( true );
    cout << "\tGates " << endl;
    for(int i = 0; i < this->gates.size(); i++)
    {
      cout << "\t  " << this->gates[i].first.first << " " << this->gates[i].first.second << endl;
    }
  #endif
}

void Map::ParseMap( string mapStr )
{
  int line, col, mapLen = this->length;

  for(int i = 0, len = mapStr.size(); i < len; i++ )
  {
    line = ( i / mapLen ) + 1;
    col  = ( i % mapLen ) + 1;

    switch( mapStr[i] )
    {
      case 'G': map[line][col] = Map::GRASS;    break;
      case 'F': map[line][col] = Map::FOREST;   break;
      default : map[line][col] = Map::INF;      break; /* set for dark dungeon area */
    }
  }
}

void Map::Display( bool showCost )
{
  for(int i = 1; i <= this->length; i++ )
  {
    for(int j = 1; j <= this->length; j++ )
    {
      char code = '-';
      switch( map[i][j] )
      {
        case Map::GRASS    : code = 'G'; break;
        case Map::FOREST   : code = 'F'; break;
        default            : code = '-';
      }

      cout << code;
      if( showCost )
        cout << "[" << map[i][j] << "] ";

    }
    cout << endl;
  }
}

State Map::Solve( Coord start , 
                  Coord goal )
{
 //bool (*fnPt)(State , State) = fncomp;
  priority_queue< State , vector< State > , SetCompare > pq ; /* Priority queue */
  bool over = false;
  vector<bool> visited( this->length * this->length + 1 , false ); /* Visited nodes are marked; w/ margin */
  State solution;

  #ifdef DEBUG
    cout << "Solving for:" << endl;
    cout << "\t (" << start.first << ", " << start.second << ") ";
    cout << "-> (" << goal.first << ", " << goal.second << ")" << endl;
  #endif

  State initial = make_pair( 
    make_pair( ManhattanDistance( start , goal ) , 0 ) ,
    Path( 1, start ) 
  );
  pq.push(initial);

  while( !over && !pq.empty() )
  {
    /* Get the current path with least cost, and
        remove it from our "heap" */
    State top = pq.top();
    pq.pop();

    /* Get the last visited node of the candidate path */
    Coord last = *(top.second.rbegin());

    #ifdef DEBUG
      cout << "(" << last.first << "," << last.second << ") [" << top.first.second << ", " << top.first.first << "]" << endl;
    #endif
    
    /* Identify possible new expansions*/
    vector< Coord > candidates = ExpandFrontier( last );    

    for( int i = 0, len = candidates.size(); i < len ; i++)
    {
      int col  = candidates[i].first,
          line = candidates[i].second,
          idx  = ( line - 1 ) * (this->length) + col ;

      /* Adding to the heap paths which do not take us back
          to a previously seen node; if the goal state is reached,
          exit the loop and return it */ 
        #ifdef DEBUG
          cout << "\t@(" << col << "," << line << ") ";
        #endif

      if( !visited[ idx ] )
      {
        visited[idx] = true;

        /* stepsCost is the sum of costs of traversed edges from start to candidate
           totalCost is the aggregate of: 
            stepsCost + 
            Manhattan Distance from candidate to goal */
        int stepsCost = top.first.second + map[line][col],
            totalCost = stepsCost + ManhattanDistance( candidates[i] , goal ) ;

        #ifdef DEBUG
          cout << "[" << stepsCost << ", " << totalCost << "]"  ;
        #endif 

        Path newPath( top.second.begin() , top.second.end() );
        newPath.push_back( candidates[i] );

        State next( make_pair( totalCost , stepsCost ), newPath );
        
        /* check if we reached the goal */
        if( candidates[i] == goal )
        {
          solution = next;
          over = true;
          break;
        }

        pq.push( next );
      }
      #ifdef DEBUG
        cout << endl;
      #endif
    }
  } 

  #ifdef DEBUG
    cout << "\nSolution found...\n";

  #endif

  return solution;
}

/* Class API */

Map::Direction Map::relativeDirection( Coord from ,
                                       Coord to    )
{
  Coord diff = std::make_pair( 
    to.first - from.first   ,
    to.second - from.second 
  );

  /* UP */
    if( diff.second <= -1 )
      return Map::POS_UP;  
  /* RIGHT */
    else if( diff.first >= 1 )
      return Map::POS_RIGHT;  
  /* DOWN */
    else if( diff.second >= 1 )
      return Map::POS_DOWN;
  /* LEFT */
    else if ( diff.first <= -1 )
      return Map::POS_LEFT;

}

bool Map::isAdjacent( Coord pos1 , 
                      Coord pos2  )
{
  Coord diff = std::make_pair( 
    pos2.first  - pos2.first   ,
    pos2.second - pos2.second 
  );

  if( diff.first >= -1  && diff.first <= 1 &&
      diff.second >= -1 && diff.second <= 1  )
    return true;

  return false;
}


/* Private Helpers */

int Map::ManhattanDistance( Coord from ,
                            Coord to )
{
  return abs( to.first - from.first ) + abs( to.second - from.second );
}

bool Map::IsValidCoord( Coord pos )
{
  if( pos.first  < 1 || pos.first > this->length  ||
      pos.second < 1 || pos.second > this->length  )
      return false;
  return true;
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

    if( IsValidCoord( neighbor ) &&
        map[ neighbor.second ][ neighbor.first ] != Map::INF )
      valid_neighs.push_back( neighbor ) ;
  }

  return valid_neighs;
}



