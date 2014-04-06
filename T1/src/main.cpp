#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include "CPUTimer.h"
#include "map_config.h"
#include "map.h"
#include "utils.h"

#define INFINITY 100000000

using namespace std;

int main(){
  CPUTimer timer;

  string inputFiles[] = {
    "../maps/dun1.txt" ,
    "../maps/dun2.txt" ,
    "../maps/dun3.txt" ,
    "../maps/mapa0.txt"
  };

  /* Load the maps from the files */
  vector<Map> maps;
  for( int i = 0; i < 4 ; i++ )
  {
    MapConfig config = Utils::ReadFile( inputFiles[i].c_str() );
    Map map( config , ( 
      inputFiles[i].find("dun") != string::npos ?
        true : false 
      )
    );

    maps.push_back( map );
  }
    

  /* Now find the best overall path */  
  vector< State > dungeons, overworld;
  int perms[ 6 ][ 5 ] = {
    {0, 1, 2, 3, 4}, 
    {0, 1, 3, 2, 4}, 
    {0, 2, 1, 3, 4}, 
    {0, 2, 3, 1, 4}, 
    {0, 3, 1, 2, 4}, 
    {0, 3, 2, 1, 4}
  };
  int bestPermIdx = 0;

  timer.start();
  for( int i = 0, len = maps.size(); i < len; i++)
  {
    /* for a dungeon we just have two endpoints, go from one to the other */
    if( maps[i].isDungeon )
    {
      dungeons.push_back(
        maps[i].Solve( maps[i].gates[1].first , maps[i].gates[0].first )
      );
    }

    /* solve for overworld:
         - we know the start and end positions
         - test different permutations of dungeons
    */
    else 
    {
      int minCost = INFINITY;      
     
      for( int j = 0; j < 6; j++)
      {

        int curCost = 0;
        vector< State > tentativeSolution;

        /* Find out the best paths for a given permutation;
           remember the total cost */
        for( int k = 0; k < 4; k++)
        {
          State partialSolution = maps[i].Solve( 
            maps[i].gates[ perms[ j ][ k ] ].first ,
            maps[i].gates[ perms[ j ][ k + 1 ] ].first 
          );

          /* take into account the steps cost of this path */
          curCost += partialSolution.first.second ; 

          tentativeSolution.push_back( partialSolution ) ;
        }

        /* if we have found a cheaper path, set it as the best solution */
        if( curCost < minCost )
        {
          bestPermIdx = j ;
          minCost = curCost ; 
          overworld.assign(tentativeSolution.begin(), tentativeSolution.end());
        }
        
        #ifdef DEBUG
          cout << "Permutation: {";
          for( int c = 0; c < 5; c++ )
            cout << ((c > 0) ? ", " : "") << perms[j][c] ;
          cout << "}" << endl;
          cout << "\tcost: " << curCost << endl;

          cout << "\toverworld [ " << curCost << " ] : \n";
          for( int c = 0, tam = tentativeSolution.size(); c < tam; c++)
          {
            cout << "cost: " << tentativeSolution[c].first.second << endl;
            cout << Utils::CoordsToString( tentativeSolution[c].second ) << endl;
          }
          
          cout << "\tdungeons: \n";
          for( int c = 0, tam = dungeons.size(); c < tam; c++)
          {
            cout << "cost: " << dungeons[c].first.second << endl;
            cout << Utils::CoordsToString( dungeons[c].second ) << endl;
          }
        #endif
      }
    }

    
  }
  timer.stop();

  #ifdef DEBUG
    cout << "Best permutation: \n";
    for( int i = 0; i < 5; i++ )
      cout << perms[ bestPermIdx ][ i ] << " ";
    cout << endl << endl;

    cout << "Number of paths\n";
    cout << "\t@overworld: " << overworld.size() << endl;
    cout << "\t@dungeons : " << dungeons.size() << endl;
  #endif

  #ifdef DEBUG
    int cost = 0;
  #endif

  for( int i = 0, len = overworld.size(); i < len; i++)
  {
    ios_base::openmode openMode = fstream::out | (
      ( i > 0 ) ? fstream::app : fstream::out 
    );

    /* log the each path for the overworld */
    Utils::LogSolution("../logs/solution.log", "", 
      Utils::CoordsToDirections( overworld[i].second ) , openMode );
    
    #ifdef DEBUG
      Coord begin = *(overworld[i].second.begin()),
            end   = *(overworld[i].second.rbegin());

      cout << "(" << begin.first << ", " << begin.second << ") -> ";
      cout << "(" << end.first   << ", " << end.second   << ")\n";
      cout << "\tcost: [" << overworld[i].first.second << ", " << overworld[i].first.first << "]\n";

      cost += overworld[i].first.second;
    #endif

    /* append dungeon path */
    if( i < len - 1 )
    {
      Utils::LogSolution("../logs/solution.log", "", 
        Utils::CoordsToDirections( dungeons[ perms[ bestPermIdx ][ i+1 ] - 1 ].second ) , (fstream::out | fstream::app) );
      
      #ifdef DEBUG
        cout << "dg size: " << dungeons[ perms[ bestPermIdx ][ i+1 ] - 1 ].second.size() << endl;
        cout << "dg cost: " << 2*dungeons[ perms[ bestPermIdx ][ i+1 ] - 1 ].first.second << endl;
        cost += 2 * dungeons[ perms[ bestPermIdx ][ i+1 ] - 1 ].first.second;
      #endif
    }
  } 

  #ifdef DEBUG
    cout << "Overall cost: " << cost << endl;
  #endif

  #ifdef TIMER
    cout << "Time: " << timer.getCPUTotalSecs() << endl;
  #endif

  return 0;
}



