#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include "utils.h"
#include "map_config.h"

string Utils::InvertDirections( string dirs )
{
  stringstream invertedDirs;

  for( int i = 0, len = dirs.size(); i < len; i++)  
  {
    
    switch( (Utils::Directions) ( dirs[i] - '0' ) )
    {
      case UP   : invertedDirs << DOWN ; break;
      case RIGHT: invertedDirs << LEFT ; break;
      case DOWN : invertedDirs << UP   ; break;
      case LEFT : invertedDirs << RIGHT; break;
      default   : invertedDirs << "";
    }
  }

  return invertedDirs.str();
}

string Utils::CoordsToDirections ( Path path )
{
  stringstream dirs;

  for(int i = 0, len = path.size() - 1; i < len; i++  )
  {
    Coord diff = make_pair( 
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

string Utils::CoordsToString ( Path path )
{
  stringstream str;

  for(int i = 0, len = path.size(); i < len; i++  )
  {
    if( i > 0 )
      str << " -> ";
    
    str << "(" << path[i].first << ", " << path[i].second << ") ";
  }
  str << "\n";

  return str.str();
}


MapConfig Utils::ReadFile(const char * fileName)
{
  string line, input = "";
  stringstream rem_ws; /* used to remove whitespaces */
  ifstream mapFile (fileName);
  int numberOfGates;
  MapConfig inputMap;

  if (mapFile.is_open())
  {
    /* Read map id, number of gates and width x height */
    mapFile >> inputMap.id >> numberOfGates >> inputMap.width >> inputMap.height ;

    /* Read all the gates */
    for( int i = 0 ; i < numberOfGates ; i++ )
    {
      Gate g;     
      int col, line;
      
      /* col, line, id (for map or entity) */
      mapFile >> col >> line >> g.second ;

      g.first.first = col + 1;
      g.first.second = line + 1;

      inputMap.gates.push_back( g );
    }

    /* Now read the map itself */
    while ( getline (mapFile, line) )
    {
      stringstream rem_ws( line );

      while( rem_ws >> line )
        inputMap.str.append( line );
    }

    mapFile.close();
  }

  return inputMap;
}


bool Utils::LogSolution(const char * fileName, string header,
                        string output, ios_base::openmode openMode )
{
  fstream  logFile;
  logFile.open ( fileName, openMode );

  if(!logFile.is_open())
    return false;
  if( header != "" )
    logFile << header << endl;

  /* first, convert to directions */
  logFile << output << endl;

  logFile.close();

  return true;
}