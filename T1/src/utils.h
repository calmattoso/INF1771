#ifndef _UTILS_H_
#define _UTILS_H_

#include <string>
#include <vector>

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


	static string CoordsToDirections ( vector< pair<int,int> > path );

public:

	static string ReadFile ( const char * );

	static bool   LogSolution ( const char * , string , vector< pair<int,int> > );
};

#endif