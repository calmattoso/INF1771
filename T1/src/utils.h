#ifndef _UTILS_H_
#define _UTILS_H_

#include <string>
#include <vector>

using namespace std;

string ReadFile(const char *);

bool LogSolution(const char * , vector< pair<int,int> > );

#endif