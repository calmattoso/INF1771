% ------------------------------------------------------------------------------
%
%   MODULE - **problem** definition ( alias: p )
%
% ------------------------------------------------------------------------------

  :- module( p , [
 	  	at/2,
 	  	visited/1,
 	  	pos/2,
 	  	item/1,
 	  	safe/1,
 	  	energy/2,
 	  	is_dead/0,
 	  	adjust_safe/0,
 	  	mark_unsafe/0,
 	  	is_adjacent/2,
 	  	get_adjacent/3,
 	  	sensed/1
 	  ]).

	:- dynamic 
		at/2,
		visited/1,
		safe/1,
		energy/2,
		sensed/1.

% ------------------------------------------------------------------------------
%
%   EXPORTED PREDICATES
%
% ------------------------------------------------------------------------------

	% ----------------------------------------------------------------------------
	%  Description
	%    Checks if two positions are adjacent.
	% ----------------------------------------------------------------------------

		is_adjacent( Pos1 , Pos2 ) :-
			get_adjacent( _ , AdjPos , Pos1 ), AdjPos , AdjPos == Pos2.

	% ----------------------------------------------------------------------------
	%  Description
	%    Adjusts positions surrounding the agent`s current position to safe.
	% ----------------------------------------------------------------------------

		mark_unsafe :-
			at( agent , Pos ),
			( test_unsafe( spatial_distortion , Pos ) ; true),
			( test_unsafe( noises , Pos ) ; true ),
			( test_unsafe( breeze , Pos ) ; true ).			

	% ----------------------------------------------------------------------------
	%  Description
	%    Adjusts positions surrounding the agent`s current position to safe.
	% ----------------------------------------------------------------------------

		adjust_safe :-
			at( agent , Pos ),
			get_adjacent( _ , NewPos , Pos) , NewPos , 
			( not(safe( NewPos )), assertz(safe(NewPos))).

	% ----------------------------------------------------------------------------
	%  Description
	%    Checks if the agent has died.
	% ----------------------------------------------------------------------------

		is_dead :-
			energy( agent , Energy ),
			Energy =< 0.

	% ----------------------------------------------------------------------------
	%  Description
	%    Returns the position adjacent to pos(X,Y) at a given direction
	% ----------------------------------------------------------------------------

		get_adjacent( north , pos(X , NewY) , pos(X,Y) ) :-
			NewY is Y-1 , pos(X, NewY).

		get_adjacent( south , pos(X , NewY ) , pos(X,Y) ) :-
			NewY is Y+1, pos(X, NewY).

		get_adjacent( east , pos( NewX , Y ) , pos(X,Y) ) :-
			NewX is X+1, pos(NewX,Y).

		get_adjacent( west , pos( NewX , Y ) , pos(X,Y) ) :-
			NewX is X-1, pos(NewX,Y).

% ------------------------------------------------------------------------------
%
%   HELPERS
%
% ------------------------------------------------------------------------------

	% ----------------------------------------------------------------------------
	%  Description
	%    Marks adjacent positions (not known to be safe) of given Pos to have
	%			 specific danger if its respective danger warning exists at Pos.
	%	
	%	 Effects
	%    Below is displayed which mark is made on AdjPos based on a danger alert:
	%		   | spatial_distortion -> potencial_vortex  |
	%      | ............noises -> potencial_monster |
	%      | ............breeze -> potencial_hole    |
	%    If a potencial_{danger} already exists at a given position, we assume 
	%		   such position actually has that danger.
	%		 As a result the potencial_{danger} is removed and the actual {danger} is
	%			 asserted.
	%		
	% ----------------------------------------------------------------------------

		% Vortex %
			test_unsafe( spatial_distortion , Pos ) :-				
				at( spatial_distortion , Pos ),
					retract( at(spatial_distortion , Pos)),
				bagof( AdjPos , get_adjacent(_, AdjPos , Pos ), [Head]),
					not( safe( Head )), (
						( not(at(potencial_vortex , Head)), 
							assertz(at(potencial_vortex , Head)) );
						( retract(at(potencial_vortex , Head)) ,
							assertz(at(vortex , Head)) )
					).

		
		% Monster %
			test_unsafe( noises , Pos ) :-
				at( noises , Pos ),
					retract( at(noises , Pos)),
				bagof( AdjPos , get_adjacent(_, AdjPos , Pos ), [Head]),
					not( safe( Head )), (
						( not(at(potencial_monster , Head)), 
							assertz(at(potencial_monster , Head)) );
						( retract(at(potencial_monster , Head)) ,
							assertz(at(monster , Head)) )
					).	

		% Hole %
			test_unsafe( breeze , Pos ) :-
				at( breeze , Pos ),
					retract( at(breeze , Pos)),
				bagof( AdjPos , get_adjacent(_, AdjPos , Pos ), [Head]),
					not( safe( Head )), (
						( not(at(potencial_hole , Head)), 
							assertz(at(potencial_hole , Head)) );
						( retract(at(potencial_hole , Head)) ,
							assertz(at(hole , Head)) )
					).	



% ------------------------------------------------------------------------------
%
%   FACTS
%
% ------------------------------------------------------------------------------

	% ----------------------------------------------------------------------------
	%  Definition of the domain of valid positions (grass squares).
	% ----------------------------------------------------------------------------

		pos( 20 , 38 ).
		pos( 20 , 39 ).
		pos( 20 , 40 ).
		pos( 21 , 38 ).
		pos( 21 , 39 ).
		pos( 21 , 40 ).
		pos( 22 , 38 ).
		pos( 22 , 39 ).
		pos( 22 , 40 ).
		pos( 23 , 38 ).
		pos( 23 , 39 ).
		pos( 23 , 40 ).
		pos( 24 , 38 ).
		pos( 24 , 39 ).
		pos( 24 , 40 ).
		pos( 25 , 38 ).
		pos( 25 , 39 ).
		pos( 25 , 40 ).

	% ----------------------------------------------------------------------------
	%  Definition of where entities are on the map.
	% ----------------------------------------------------------------------------

		at( agent , pos(22 , 39) ). % Agent starts here %
		at( spatial_distortion , pos(22,39)).
		at( breeze , pos(22,39)).
		at( noises , pos(22,39)).

		at( rupee_glow , pos(21 , 38) ).
		at( fairies , pos(22, 39) ).
		at( pendants_glow , pos( 20 , 38 )).

	% ----------------------------------------------------------------------------
	%  Valid items.
	% ----------------------------------------------------------------------------

		item( rupee ).
		item( heart ).
		item( sword ).

	% ----------------------------------------------------------------------------
	%  Initial state of the agent.
	% ----------------------------------------------------------------------------

		safe( pos( 21 , 38 ) ).
		visited( pos(21 , 38) ).		
		energy( agent , 100 ).



