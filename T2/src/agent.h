#ifndef _AGENT_H_
#define _AGENT_H_

#include <vector>

#include "map_config.h"
#include "map.h"

class Agent 
{
public:
	enum Actions
	{
		TURN_RIGHT,
		TURN_LEFT,
		GO_AHEAD,
		ATTACK,
		GET_SWORD,
		GET_HEART,
		GET_RUPEE,
		FALL_IN_HOLE,
		GET_ATTACKED
	};

private:

	Coord position;
	
	Map::Direction direction;

	int cost;

	uint8_t energy;
	uint8_t maxEnergy;

#ifdef LOG
	std::vector< std::string > actionsLog;
#endif

private:

	int getActionCost( Agent::Actions );

	std::string actionToString( Agent::Actions );

public:

	Agent( Coord , Map::Direction , uint8_t , uint8_t );

	bool isDead( );

	void increaseEnergy( uint8_t );

	void decreaseEnergy( uint8_t );

	void setEnergy( uint8_t );

	uint8_t getEnergy( );

	int getCost( );

	bool setPosition( Coord );

	Coord getPosition( );

	void setDirection( Map::Direction );

	Map::Direction getDirection( );

	void doAction( Agent::Actions );

	std::string toString( );

};

#endif
