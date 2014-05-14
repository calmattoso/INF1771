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

void adjust_string(string& str, string hint);
void check_danger(string dangerCode , ofstream& log);

int main( )
{
  char * argv[] = {
    "swipl.dll",
    "-s", "..\\src\\prolog\\run.pl",
    "-q",
    NULL
  };
  int argc = 4;
  bool not_over = true;
  ofstream log_prolog("../logs/prolog.log"),
           log_vis("../logs/vis.log");
  string dangerCodes[6] = {
    "potential_monster", "potential_hole", "potential_vortex",
    "actual_monster", "actual_hole", "actual_vortex" 
  };

  if( !log_vis.is_open() 
    #ifdef _LOG_PROLOG
      && !log_prolog.is_open() 
    #endif
  ){
    cout << "Could not open log files! Terminating...\n";
    exit(1);
  }

  _putenv( SWIPL_ENV );

  PlEngine e( argc , argv ); 

  while( not_over )
  {
    bool gone_wrong = false;

    /* Link's gotta sense something */
      PlTermv * senseTerm = new PlTermv(1);
      PlQuery * senseQuery = new PlQuery("sense", *senseTerm);

      if( senseQuery->next_solution() )
      {
        /* display what sense yielded, and store that */
          string senseOutcome( (( char* ) (*senseTerm)[0]) );

        #ifdef _LOG_PROLOG
          log_prolog << "sense(X)\n" << "  X = " << senseOutcome << "\n";
          log_prolog << senseOutcome << ".\n";
        #endif

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
        gone_wrong = true;

        delete senseQuery;  
        delete senseTerm;
      }

    /* Show what's safe, potencial danger and actual danger */
      PlTermv * safeTerm  = new PlTermv(1);
      PlQuery * safeQuery = new PlQuery("safe", *safeTerm);

        while( safeQuery->next_solution() )
        {
          string safePos( (( char* ) (*safeTerm)[0]) );

          adjust_string( safePos , "safe(void," );

          log_vis << safePos << "\n";
        }

      delete safeTerm;
      delete safeQuery;

      for( int i = 0; i < 6; i++ )
        check_danger( dangerCodes[i] , log_vis);      


    /* Now, Link's gotta do some action */
      PlTermv * actionTerm  = new PlTermv(1);
      PlQuery * actionQuery = new PlQuery("best_action", *actionTerm);

      if( actionQuery->next_solution() )
      {
        /* display which action best_action yielded, and store that */
          string actionOutcome( (( char* ) (*actionTerm)[0]) );
          string visString = actionOutcome;

          adjust_string(visString,"");

        #ifdef _LOG_PROLOG
          log_prolog << "best_action(X)\n" << "  X = " << actionOutcome << "\n";
        #endif
          log_vis << visString << "\n";

          if( actionOutcome == "won" || 
              actionOutcome == "like_omg_dead" )
            break;

        #ifdef _LOG_PROLOG
          log_prolog << actionOutcome << ".\n";
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
        if( gone_wrong )
        {
          cout << "Something has gone terribly wrong... :(\n";
          break;
        }

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


void adjust_string(string& str, string hint)
{

  /* Move */
    if(str.find("move(") != string::npos)
      str = "move_to(void," + str.substr(5, string::npos);

  /* Attack monster */
    else if(str.find("attack_monster(") != string::npos)
      str = "attack_monster(void," + str.substr(15, string::npos);

  /* We receive simply a position, adjust it to safe */
    else if(str.find("pos(") != string::npos)
        str = hint +  str.substr(4, string::npos);

  /* Game Won */
    else if( str == "won" )
      str = "won(void,void,void)";

  /* Game Lost */
    else if( str == "like_omg_dead")
      str = "dead(void,void,void)";
}

void check_danger(string dangerCode , ofstream& log)
{
  PlTermv term(2);
  term[0] = PlCompound( dangerCode.c_str() );
  PlQuery query("at", term);

  string hint = "actual_danger(";
  if( dangerCode.find("potential") != string::npos )
  {
    hint = "potential_danger(";
  }

  if( dangerCode.find("monster") != string::npos )
    dangerCode = "monster";
  else if( dangerCode.find("vortex") != string::npos )
    dangerCode = "vortex";
  else
    dangerCode = "hole";

  hint = hint + dangerCode + ",";

  while( query.next_solution() )
  {
    string pos( ( char* )(term[1]) ); 

    adjust_string( pos , hint );

    log << pos << "\n";
  }
}
