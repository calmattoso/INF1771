#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <set>

#include <cstdio>
#include <cstdlib>

using namespace std;

unsigned char map[44][44];
set<string> entitySet;

/* Prototypes */
  
  /* Helpers */
    string getEntityString( char code );
    void addWarnings( char code , uint32_t col , uint32_t line );

  /* Input parser */
    bool read_map(string path);
    bool read_items(string path);

  /* Prolog generators */
    bool log_map(ofstream& logFile);
    bool log_items(ofstream& logFile);

/* End of Prototypes */

    
int main()
{
  ofstream logFile("../data/map_definition.txt");

  if( logFile.is_open() )
  {
    /* Make the map be all forest */
    for(int line = 0; line < 44; ++line)
      for(int col = 0; col < 44; ++col)
      {
        map[line][col] = 'f';
      }

    read_map("../data/map.txt");
    log_map( logFile );

    read_items("../data/items.txt");
    log_items( logFile );
  }
  else
  {
    cout << "Couldn't open log file! Terminating...\n";
    exit( 1 );
  }
    
  return 0;
}

/* Input parsers */

  /* Read the map of the labyrinth */
    bool read_map( string path )
    {
      ifstream mapFile( path.c_str() );           
      string line;

      if (mapFile.is_open())
      {
        uint32_t l = 1, c = 1;

        while ( getline (mapFile,line) &&
                l >= 1 && l <= 42 )
        {
          /* New line, go back to first column */
            c = 1;

          /* For each read char of a given line */
          for(
            uint8_t k = 0, len = line.size(); 
            k < len && c >= 1 && c <= 42 ; 
            ++k
          )
          {
            switch( line[k] )
            {
              case 'f': 
              {
                map[l][c] = 'f';
                ++c;
                break;
              }
              case 'g':
              {
                map[l][c] = 'g';
                ++c;
                break;
              }

              default: break;
            }
          }

          ++l;
        }
        
        mapFile.close();

        return true;
      }

      return false;
    }

  /* Read the items that will be on the map */
    bool read_items( string path )
    {
      ifstream itemsFile( path.c_str() );
      string line;

      if( itemsFile.is_open() )
      {
        string entityDef;

        while( getline(itemsFile , line) )
        {
          uint32_t l, c;
          char entityCode[3];
          stringstream output;

          sscanf(line.c_str(), " %u %u %s", &l, &c, entityCode);
          ++l; ++c;

          string entityString = getEntityString( entityCode[0] );
          if( entityString != "" )
          {
            output << "at(" << entityString << ",pos(" << c << "," <<  l << ")).";

            entitySet.insert( output.str() );
          }

          /* Remember where there is the actual master sword */
            if( entityCode[0] == 'M' )
            {
              output.str( string() );
              output.clear();

              output << "at(master_sword,pos(" << c << "," <<  l << ")).";

              entitySet.insert( output.str() );
            }

          /* Add warnings at positions surrounding a monster/vortex/hole */
            else if( entityCode[0] == 'B' || entityCode[0] == 'V' ||
                     entityCode[0] == 'E'   )
            {
              addWarnings( entityCode[0] , c , l );
            }
        }

        itemsFile.close();

        return true;
      }

      return false;
    }

/* End of Input parsers */


/* Prolog generators */
  
  /* Output the Prolog definitions of where there is grass */
    bool log_map( ofstream& logFile )
    {

      if( logFile.is_open() )
      {
        for(int line = 1; line <= 42; line++)
          for(int col = 1; col <= 42; col++)
            if( map[line][col] == 'g' )
              logFile << "pos(" << col << "," << line << ").\n";      
        logFile << "\n";

        return true;
      }  

      return false;
    }

  /* Output the Prolog definitions of where there are entities */
    bool log_items( ofstream& logFile )
    {
      if( logFile.is_open() )
      {
        set<string>::iterator it;
        for(it = entitySet.begin(); it != entitySet.end(); ++it)
          logFile << *it << "\n";

        return true;
      }
      return false;
    }

/* End of Prolog generators */

/* Helpers */
  
  /* For each possible entity return the string that should be assigned to its
       respective position */
    string getEntityString( char code )
    {
      switch( code )
      {
        case 'E':
          return "monster";
        case 'V':
          return "vortex";
        case 'B':
          return "hole";
        case 'M':
        case 'F':
          return "pendants_glow";
        case 'C':
          return "fairies";
        case 'R':
          return "rupee_glow";
        default:
          return "";
      }
    }

  /* Add warnings to the entity set in positions around a monster, vortex or
       a hole. */
  void addWarnings( char code , uint32_t col , uint32_t line )
  {
    string warning = "";
    stringstream output;

    int32_t offsets[ 4 ][ 2 ] = {
      { 1,  0}, /* east  */
      {-1,  0}, /* west  */
      { 0,  1}, /* south */
      { 0, -1}  /* north */
    };

    /* First check what kind of danger we got and set the string correctly */
      
      switch( code )
      {
        case 'E':
          warning = "noises";
          break;
        
        case 'V':
          warning = "spatial_distortions";
          break;
        
        case 'B':
          warning = "breeze";
          break;
        
        default: return;
      }

    /* Now insert warnings on valid positions into the entity set */
      for( int i = 0; i < 4; i++ )    
      {
        uint32_t col_offset  = offsets[i][ 0 ],
                 line_offset = offsets[i][ 1 ] ;
        stringstream output;

        if( map[ line + line_offset ][ col + col_offset ] == 'g' )
        {
          output << "at(" << warning << ",pos(" << col + col_offset
                 << "," <<  line + line_offset << ")).";

          entitySet.insert( output.str() );
        }
      }    
  }

/* End of Helpers */
