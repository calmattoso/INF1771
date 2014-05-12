#include <iostream>
#include <fstream>
#include <string>
#include <SWI-cpp.h>

#define _LOG_PROLOG 

#ifdef _WINDOWS
  #include "swipl_env_win32.h"
#else
  #include "swipl_env_lnx.h"
#endif

using namespace std;

int main( )
{
  char * argv[] = {
    "libswipl.dll",
    "-s", "prolog\\run.pl",
    "-q",
    NULL
  };
  int argc = 4;
  bool not_over = true;
  ofstream log_prolog("../logs/prolog.log"),
           log_vis("../logs/vis.log");

  if( !log_prolog.is_open() && !log_vis.is_open() )
  {
    cout << "Could not open log files! Terminating...\n";
    exit(1);
  }

  _putenv( SWIPL_ENV );

  PlEngine e( argc , argv ); 

  while( not_over )
  {
    /* Link's gotta sense something */
      PlTermv * senseTerm = new PlTermv(1);
      PlQuery * senseQuery = new PlQuery("sense", *senseTerm);

      if( senseQuery->next_solution() )
      {
        /* display what sense yielded, and store that */
          string senseOutcome( (( char* ) (*senseTerm)[0]) );

#ifdef _LOG_PROLOG
          log_prolog << "sense(X)\n" << "  X = " << senseOutcome << "\n";
          log_prolog << senseOutcome << "\n";
#endif
          log_vis << senseOutcome << "\n";

        /* destroy the query and term */
          delete senseQuery;
          delete senseTerm;

        /* do the action sense has told us to*/
          PlTermv callTerm(1);
          callTerm[0] = PlCompound( senseOutcome.c_str() );
          PlQuery callQuery("call", callTerm);

          while ( callQuery.next_solution() );
      } 
      else
      {
        delete senseQuery;  
        delete senseTerm;
      }

    /* Now, Link's gotta do some action */
      PlTermv * actionTerm  = new PlTermv(1);
      PlQuery * actionQuery = new PlQuery("best_action", *actionTerm);

      if( actionQuery->next_solution() )
      {
        /* display which action best_action yielded, and store that */
          string actionOutcome( (( char* ) (*actionTerm)[0]) );

#ifdef _LOG_PROLOG
          log_prolog << "best_action(X)\n" << "  X = " << actionOutcome << "\n";
#endif
          log_vis << actionOutcome << "\n";

          if( actionOutcome == "won" || 
              actionOutcome == "like_omg_dead" )
            break;

#ifdef _LOG_PROLOG
          log_prolog << actionOutcome << "\n";
#endif


        /* destroy the query and term */
          delete actionQuery;
          delete actionTerm;

        /* do the action best_action has told us to*/
          PlTermv callTerm(1);
          callTerm[0] = PlCompound( actionOutcome.c_str() );
          PlQuery callQuery("call", callTerm);

          while ( callQuery.next_solution() );
      }
      else 
      {
        delete actionQuery;
        delete actionTerm;
      }    
  }

#ifdef _LOG_PROLOG
  log_prolog.close();
#endif
  log_vis.close();

  return 0;
}



