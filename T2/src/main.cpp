#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>

#include "map_config.h"
#include "map.h"
#include "utils.h"

#define INFINITY 100000000

using namespace std;

int main( int argc , char const *argv[] )
{
  CPUTimer timer;

  string inputFiles[] = {
    "../data/map.txt",
    "../data/itens.txt"
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

  return 0;
}



