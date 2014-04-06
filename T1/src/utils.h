#ifndef _UTILS_H_
#define _UTILS_H_

#include <string>
#include <vector>
#include <fstream>
#include "map_config.h"

using namespace std;

class Utils 
{
private:
	
	enum Directions {
		UP    = 1,
		RIGHT = 2,
		DOWN  = 3,
		LEFT  = 4
	};


public:

	static string CoordsToDirections ( Path path );

	static string InvertDirections( string dirs ) ;

	static string CoordsToString ( Path path );

	static MapConfig ReadFile ( const char * );

	static bool   LogSolution ( const char * , string , string , ios_base::openmode );
};

#endif