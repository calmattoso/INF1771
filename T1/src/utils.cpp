#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include "utils.h"

string ReadFile(const char * fileName)
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


bool LogSolution(const char * fileName, string header,
                 vector< pair<int,int> > path )
{
  ofstream logFile (fileName);

  if(!logFile.is_open())
    return false;

  logFile << header << endl;
  for (int i = 0; i < path.size(); ++i)
  {
    if(i > 0)
      logFile << ";";

    logFile << "(" << path[i].first << "," << path[i].second << ")";
  }

  logFile.close();
  return true;
}