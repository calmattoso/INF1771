header = [[
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
 	  	has_won/0,
 	  	is_dead/0,
 	  	adjust_safe/0,
 	  	mark_unsafe/0,
 	  	is_adjacent/2,
 	  	get_adjacent/3,
 	  	get_adjacent_list/3,
 	  	sensed/2,
 	  	on_vortex/0,
 	  	check_local/0
 	  ]).

	:- dynamic 
		at/2,
		visited/1,
		safe/1,
		energy/2,
		sensed/2,
		on_vortex/0,
		has_won/0.

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
	%    Corrects the agent`s knowledge about its current position.
	% ----------------------------------------------------------------------------

		check_local :-
			at( agent , Pos ),
			( update_danger_inferences( 
					Pos , vortex , actual_vortex , potencial_vortex
				);
			  true),
			( update_danger_inferences( Pos , hole , actual_hole , potencial_hole );
			  true),
			( update_danger_inferences( 
					Pos , monster , actual_monster , potencial_monster 
				);
			  true).  			 		


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
				get_adjacent_list( _ , Pos , [Head|Tail]),
					(
						( length(Tail , 0) ,
							assertz( at(actual_vortex, Head) ));
						iterate_adjacent_list( potential_vortex , actual_vortex , Tail )
					).	
		
		% Monster %
			test_unsafe( noises , Pos ) :-
				at( noises , Pos ),
					retract( at(noises , Pos)),
				get_adjacent_list( _ , Pos , [Head|Tail]),
					(
						( length(Tail , 0) ,
							assertz( at(actual_monster, Head) ));
						iterate_adjacent_list( potential_monster , actual_monster , Tail )
					).	

		% Hole %
			test_unsafe( breeze , Pos ) :-
				at( breeze , Pos ),
					retract( at(breeze , Pos)),
				get_adjacent_list( _ , Pos , [Head|Tail]),
					(
						( length(Tail , 0) ,
							assertz( at(actual_hole, Head) ));
						iterate_adjacent_list( potential_hole , actual_hole , Tail )
					).

	% ----------------------------------------------------------------------------
	%  Description
	%    Generates and returns list L with all, not known to be safe, adjacent 
	%		   positions to Pos.
	% ----------------------------------------------------------------------------

		get_adjacent_list( Direction , Pos , L ) :-
			findall( AdjPos , 
				(get_adjacent( Direction , AdjPos , Pos ), not(safe(AdjPos)) ),
				L
			).

	% ----------------------------------------------------------------------------
	%  Description
	%    Iterates through a list of positions and mark each one with 
	%		   PotencialDanger or Danger, depending on previous knowledge about given
	%			 position.
	%    We assume, after detecting a second potential danger of same type at 
	%		   Head that there probably is something there.
	% ----------------------------------------------------------------------------
		
		iterate_adjacent_list( _ , _ , [] ).
		iterate_adjacent_list( PotencialDanger , Danger , [Head|Tail]) :-
			(
				( not(at( PotencialDanger , Head)), 
					assertz(at(PotencialDanger , Head)) );
				( retract(at(PotencialDanger , Head)) ,
					assertz(at(Danger , Head)) )
			), iterate_adjacent_list( PotencialDanger, Danger, Tail).

	% ----------------------------------------------------------------------------
	%  Description
	%	   Updates the agent`s knowledge about its current position.
	%	
	%	 Effects
	%		
	% ----------------------------------------------------------------------------	
		
		update_danger_inferences( Pos , Danger , ActDanger , PotDanger ) :- 
			(at( Danger , Pos ),
				(( not(at(ActDanger,Pos)), asserta(at(ActDanger,Pos)) ); true),
				( at(PotDanger,Pos), retract(at(PotDanger,Pos)))
			);
			( not(at( Danger , Pos )),
					( ( not(safe(Pos)), asserta(safe(Pos)) ); true),
					( at(ActDanger,Pos), retract(at(ActDanger,Pos)) );
					( at(PotDanger,Pos), retract(at(PotDanger,Pos)) )
			).

]]

footer = [[
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

		safe( pos( 22 , 39 ) ).
		visited( pos(21 , 38) ).		
		energy( agent , 100 ).



]]