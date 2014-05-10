#include <algorithm>
#include <sstream>

#include "agent.h"
#include "map.h"
#include "map_config.h"


Agent::Agent( 
  Coord initialPosition ,
  Map::Direction direction , 
  uint8_t energy , 
  uint8_t maxEnergy 
)
{
  this->position = initialPosition;
  this->direction = direction;

  this->maxEnergy = maxEnergy;
  this->setEnergy( energy );

  this->cost = 0;
}

bool Agent::isDead( )
{
  if( this->energy <= 0 )
    return true;

  return false;
}

void Agent::increaseEnergy( uint8_t value )
{
  setEnergy( this->energy - value );
}

void Agent::decreaseEnergy( uint8_t value )
{
  setEnergy( this->energy + value );
}

void Agent::setEnergy( uint8_t newEnergy )
{
  this->energy = std::max( this->energy , this->maxEnergy );
}

uint8_t Agent::getEnergy( )
{
  return this->energy;
}

int Agent::getCost( )
{
  return this->cost;
}

bool Agent::setPosition( Coord newPosition )
{
  Map::Direction relativeDirection = 
    Map::relativeDirection( this->position , newPosition );

  /* Make sure the player only moves to a position which is
       in the direction he's turned to */
  if( relativeDirection == this->direction &&
      Map::isAdjacent( this->position , newPosition ) )
    this->position = newPosition;

  return false;
}

Coord Agent::getPosition( )
{
  return this->position;
}

void Agent::setDirection( Map::Direction newDirection )
{
  this->direction = newDirection;
}

Map::Direction Agent::getDirection( )
{
  return this->direction;
}

void Agent::doAction( Agent::Actions action )
{
  this->cost += getActionCost( action );

#ifdef LOG
  actionsLog.push_back( actionToString( action ));
#endif

}

std::string Agent::toString( )
{
  std::stringstream output;

  output << "{\n"
    << "\tenergy: " << this->energy << "\n"
    << "\tmaxEnergy: " << this->maxEnergy << "\n"
    << "\tposition: {" 
      << "\t" << this->position.first << "," 
      << "\t" << this->position.second 
      << "}\n"
    << "\tdirection: " << this->direction << "\n"
  << "}\n";

  return output.str();
}

/* Helper methods */


int Agent::getActionCost( Agent::Actions action )
{
  switch( action )
  {
    case TURN_RIGHT:
    case TURN_LEFT:
    case GO_AHEAD:
      return -1;

    case ATTACK:
      return -5;

    case GET_SWORD:
      return -100;

    case GET_HEART:
      return -10;

    case GET_RUPEE:
      return +10;

    case FALL_IN_HOLE:
    case GET_ATTACKED:
      return -10000;

    default:
      return 0;
  }
}

std::string Agent::actionToString( Agent::Actions action )
{
  switch( action )
  {

    case TURN_RIGHT:
      return "TURN_RIGHT";
    
    case TURN_LEFT:
      return "TURN_LEFT";

    case GO_AHEAD:
      return "TURN_RIGHT";

    case ATTACK:
      return "ATTACK_MONSTER";

    case GET_SWORD:
      return "GET_MASTER_SWORD";

    case GET_HEART:
      return "GET_HEART";

    case GET_RUPEE:
      return "GET_RUPEE";

    case FALL_IN_HOLE:
      return "FALL_IN_HOLE";

    case GET_ATTACKED:
      return "GET_ATTACKED_BY_MONSTER";

    default:
      return "";
  }
}


