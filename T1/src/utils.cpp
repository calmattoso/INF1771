#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include "utils.h"

string Utils::CoordsToDirections ( vector< pair<int,int> > path )
{
  stringstream dirs;

  for(int i = 0, len = path.size() - 1; i < len; i++  )
  {
    pair<int,int> diff = make_pair( 
      path[i + 1].first  - path[i].first ,
      path[i + 1].second - path[i].second
    );
    
    if( diff.second == -1 )
      dirs << UP;
    else if( diff.second == 1 )
      dirs << DOWN;
    else if( diff.first == 1 )
      dirs << RIGHT;
    else
      dirs << LEFT;
  }

  return dirs.str();
}

string Utils::ReadFile(const char * fileName)
{
  string line, input = "";
  stringstream rem_ws; /* used to remove whitespaces */
  ifstream mapFile (fileName);

  if (mapFile.is_open())
  {
    while ( getline (mapFile, line) )
    {
      stringstream rem_ws( line );

      while( rem_ws >> line )
      {
        input.append( line );
      }
    }

    mapFile.close();
  }

  return input;
}


bool Utils::LogSolution(const char * fileName, string header,
                        vector< pair<int,int> > path )
{
  ofstream logFile (fileName);

  if(!logFile.is_open())
    return false;
  if( header != "" )
    logFile << header << endl;

  /* first, convert to directions */
  logFile << CoordsToDirections( path );

  logFile.close();
  return true;
}