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
 	  	check_local/0,
 	  	should_visit/1
 	  ]).

	:- dynamic 
		at/2,
		visited/1,
		safe/1,
		energy/2,
		sensed/2,
		on_vortex/0,
		has_won/0,
		should_visit/1.

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
			(	
				(findall( AdjPos , (get_adjacent( _ , AdjPos , Pos )), L ),
				 should_visit_adjacent(L)); true
			),
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
						iterate_adjacent_list( potential_vortex , actual_vortex , [Head|Tail] )
					).	
		
		% Monster %
			test_unsafe( noises , Pos ) :-
				at( noises , Pos ),
					retract( at(noises , Pos)),
				get_adjacent_list( _ , Pos , [Head|Tail]),
					(
						( length(Tail , 0) ,
							assertz( at(actual_monster, Head) ));
						iterate_adjacent_list( potential_monster , actual_monster , [Head|Tail] )
					).	

		% Hole %
			test_unsafe( breeze , Pos ) :-
				at( breeze , Pos ),
					retract( at(breeze , Pos)),
				get_adjacent_list( _ , Pos , [Head|Tail]),
					(
						( length(Tail , 0) ,
							assertz( at(actual_hole, Head) ));
						iterate_adjacent_list( potential_hole , actual_hole ,[Head|Tail])
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
			),!.

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
			), iterate_adjacent_list( PotencialDanger, Danger, Tail),!.

		should_visit_adjacent([]).
		should_visit_adjacent( [Head|Tail] ) :-
			((not( should_visit(Head) ),
				not( visited(Head)),
			  asserta( should_visit(Head) ));true),
			should_visit_adjacent( Tail ),!.


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

% ------------------------------------------------------------------------------
%
%   MAP DEFINITION
%
% ------------------------------------------------------------------------------

pos(2,2).
pos(3,2).
pos(4,2).
pos(5,2).
pos(6,2).
pos(7,2).
pos(8,2).
pos(10,2).
pos(11,2).
pos(12,2).
pos(13,2).
pos(14,2).
pos(15,2).
pos(16,2).
pos(17,2).
pos(18,2).
pos(19,2).
pos(20,2).
pos(21,2).
pos(22,2).
pos(23,2).
pos(24,2).
pos(25,2).
pos(26,2).
pos(27,2).
pos(29,2).
pos(31,2).
pos(32,2).
pos(33,2).
pos(34,2).
pos(35,2).
pos(36,2).
pos(37,2).
pos(38,2).
pos(39,2).
pos(40,2).
pos(41,2).
pos(2,3).
pos(8,3).
pos(15,3).
pos(20,3).
pos(27,3).
pos(29,3).
pos(31,3).
pos(38,3).
pos(41,3).
pos(2,4).
pos(4,4).
pos(5,4).
pos(6,4).
pos(7,4).
pos(8,4).
pos(10,4).
pos(11,4).
pos(12,4).
pos(13,4).
pos(15,4).
pos(17,4).
pos(18,4).
pos(20,4).
pos(22,4).
pos(23,4).
pos(24,4).
pos(25,4).
pos(26,4).
pos(27,4).
pos(29,4).
pos(30,4).
pos(31,4).
pos(32,4).
pos(33,4).
pos(35,4).
pos(36,4).
pos(37,4).
pos(38,4).
pos(39,4).
pos(41,4).
pos(2,5).
pos(4,5).
pos(10,5).
pos(11,5).
pos(12,5).
pos(13,5).
pos(15,5).
pos(17,5).
pos(20,5).
pos(27,5).
pos(29,5).
pos(35,5).
pos(36,5).
pos(37,5).
pos(38,5).
pos(39,5).
pos(41,5).
pos(2,6).
pos(4,6).
pos(6,6).
pos(7,6).
pos(8,6).
pos(9,6).
pos(10,6).
pos(11,6).
pos(12,6).
pos(13,6).
pos(15,6).
pos(17,6).
pos(19,6).
pos(20,6).
pos(21,6).
pos(22,6).
pos(23,6).
pos(24,6).
pos(25,6).
pos(26,6).
pos(27,6).
pos(28,6).
pos(29,6).
pos(30,6).
pos(31,6).
pos(32,6).
pos(33,6).
pos(35,6).
pos(36,6).
pos(37,6).
pos(38,6).
pos(39,6).
pos(41,6).
pos(2,7).
pos(4,7).
pos(5,7).
pos(6,7).
pos(8,7).
pos(15,7).
pos(17,7).
pos(28,7).
pos(35,7).
pos(36,7).
pos(37,7).
pos(38,7).
pos(39,7).
pos(41,7).
pos(2,8).
pos(6,8).
pos(8,8).
pos(9,8).
pos(10,8).
pos(11,8).
pos(12,8).
pos(13,8).
pos(14,8).
pos(15,8).
pos(17,8).
pos(19,8).
pos(20,8).
pos(22,8).
pos(23,8).
pos(24,8).
pos(25,8).
pos(26,8).
pos(27,8).
pos(28,8).
pos(29,8).
pos(30,8).
pos(31,8).
pos(32,8).
pos(33,8).
pos(41,8).
pos(2,9).
pos(3,9).
pos(4,9).
pos(6,9).
pos(8,9).
pos(17,9).
pos(19,9).
pos(20,9).
pos(22,9).
pos(24,9).
pos(32,9).
pos(33,9).
pos(35,9).
pos(36,9).
pos(37,9).
pos(38,9).
pos(39,9).
pos(40,9).
pos(41,9).
pos(2,10).
pos(4,10).
pos(6,10).
pos(8,10).
pos(10,10).
pos(11,10).
pos(12,10).
pos(13,10).
pos(14,10).
pos(15,10).
pos(17,10).
pos(18,10).
pos(19,10).
pos(20,10).
pos(22,10).
pos(24,10).
pos(25,10).
pos(26,10).
pos(27,10).
pos(28,10).
pos(29,10).
pos(30,10).
pos(32,10).
pos(33,10).
pos(35,10).
pos(2,11).
pos(4,11).
pos(6,11).
pos(8,11).
pos(10,11).
pos(11,11).
pos(12,11).
pos(13,11).
pos(14,11).
pos(15,11).
pos(17,11).
pos(19,11).
pos(20,11).
pos(22,11).
pos(24,11).
pos(30,11).
pos(32,11).
pos(33,11).
pos(35,11).
pos(37,11).
pos(38,11).
pos(39,11).
pos(40,11).
pos(41,11).
pos(2,12).
pos(4,12).
pos(6,12).
pos(8,12).
pos(10,12).
pos(17,12).
pos(19,12).
pos(22,12).
pos(24,12).
pos(26,12).
pos(27,12).
pos(28,12).
pos(29,12).
pos(30,12).
pos(32,12).
pos(33,12).
pos(35,12).
pos(37,12).
pos(41,12).
pos(2,13).
pos(4,13).
pos(5,13).
pos(6,13).
pos(8,13).
pos(9,13).
pos(10,13).
pos(11,13).
pos(12,13).
pos(13,13).
pos(14,13).
pos(15,13).
pos(17,13).
pos(19,13).
pos(20,13).
pos(21,13).
pos(22,13).
pos(24,13).
pos(26,13).
pos(30,13).
pos(31,13).
pos(32,13).
pos(33,13).
pos(34,13).
pos(35,13).
pos(37,13).
pos(38,13).
pos(39,13).
pos(41,13).
pos(2,14).
pos(8,14).
pos(10,14).
pos(15,14).
pos(17,14).
pos(24,14).
pos(26,14).
pos(27,14).
pos(28,14).
pos(29,14).
pos(30,14).
pos(32,14).
pos(33,14).
pos(34,14).
pos(35,14).
pos(37,14).
pos(41,14).
pos(2,15).
pos(3,15).
pos(4,15).
pos(5,15).
pos(6,15).
pos(7,15).
pos(8,15).
pos(10,15).
pos(11,15).
pos(12,15).
pos(13,15).
pos(15,15).
pos(16,15).
pos(17,15).
pos(18,15).
pos(20,15).
pos(21,15).
pos(22,15).
pos(23,15).
pos(24,15).
pos(26,15).
pos(30,15).
pos(32,15).
pos(33,15).
pos(35,15).
pos(36,15).
pos(37,15).
pos(38,15).
pos(39,15).
pos(41,15).
pos(8,16).
pos(10,16).
pos(11,16).
pos(12,16).
pos(13,16).
pos(15,16).
pos(20,16).
pos(21,16).
pos(22,16).
pos(24,16).
pos(26,16).
pos(27,16).
pos(28,16).
pos(29,16).
pos(30,16).
pos(32,16).
pos(33,16).
pos(35,16).
pos(37,16).
pos(41,16).
pos(2,17).
pos(4,17).
pos(5,17).
pos(6,17).
pos(7,17).
pos(8,17).
pos(10,17).
pos(11,17).
pos(12,17).
pos(13,17).
pos(15,17).
pos(16,17).
pos(17,17).
pos(18,17).
pos(20,17).
pos(22,17).
pos(24,17).
pos(25,17).
pos(26,17).
pos(27,17).
pos(28,17).
pos(29,17).
pos(30,17).
pos(31,17).
pos(32,17).
pos(33,17).
pos(35,17).
pos(37,17).
pos(38,17).
pos(39,17).
pos(41,17).
pos(2,18).
pos(10,18).
pos(11,18).
pos(12,18).
pos(13,18).
pos(15,18).
pos(17,18).
pos(18,18).
pos(20,18).
pos(22,18).
pos(24,18).
pos(26,18).
pos(27,18).
pos(41,18).
pos(2,19).
pos(4,19).
pos(5,19).
pos(6,19).
pos(7,19).
pos(8,19).
pos(12,19).
pos(15,19).
pos(17,19).
pos(18,19).
pos(19,19).
pos(20,19).
pos(22,19).
pos(23,19).
pos(24,19).
pos(26,19).
pos(27,19).
pos(28,19).
pos(29,19).
pos(30,19).
pos(31,19).
pos(32,19).
pos(33,19).
pos(35,19).
pos(36,19).
pos(37,19).
pos(38,19).
pos(39,19).
pos(40,19).
pos(41,19).
pos(2,20).
pos(4,20).
pos(6,20).
pos(7,20).
pos(8,20).
pos(9,20).
pos(10,20).
pos(11,20).
pos(12,20).
pos(13,20).
pos(14,20).
pos(15,20).
pos(17,20).
pos(18,20).
pos(19,20).
pos(20,20).
pos(28,20).
pos(33,20).
pos(35,20).
pos(2,21).
pos(3,21).
pos(4,21).
pos(10,21).
pos(20,21).
pos(22,21).
pos(23,21).
pos(24,21).
pos(25,21).
pos(26,21).
pos(27,21).
pos(28,21).
pos(29,21).
pos(30,21).
pos(31,21).
pos(32,21).
pos(33,21).
pos(35,21).
pos(36,21).
pos(37,21).
pos(38,21).
pos(39,21).
pos(40,21).
pos(41,21).
pos(2,22).
pos(4,22).
pos(6,22).
pos(7,22).
pos(8,22).
pos(10,22).
pos(11,22).
pos(12,22).
pos(13,22).
pos(14,22).
pos(16,22).
pos(17,22).
pos(18,22).
pos(19,22).
pos(20,22).
pos(22,22).
pos(33,22).
pos(41,22).
pos(2,23).
pos(4,23).
pos(6,23).
pos(8,23).
pos(14,23).
pos(16,23).
pos(17,23).
pos(19,23).
pos(20,23).
pos(22,23).
pos(23,23).
pos(24,23).
pos(25,23).
pos(26,23).
pos(27,23).
pos(28,23).
pos(29,23).
pos(30,23).
pos(31,23).
pos(32,23).
pos(33,23).
pos(35,23).
pos(36,23).
pos(37,23).
pos(38,23).
pos(39,23).
pos(40,23).
pos(41,23).
pos(2,24).
pos(4,24).
pos(6,24).
pos(8,24).
pos(10,24).
pos(11,24).
pos(12,24).
pos(13,24).
pos(14,24).
pos(16,24).
pos(17,24).
pos(19,24).
pos(20,24).
pos(27,24).
pos(35,24).
pos(2,25).
pos(4,25).
pos(6,25).
pos(8,25).
pos(10,25).
pos(16,25).
pos(17,25).
pos(18,25).
pos(19,25).
pos(20,25).
pos(22,25).
pos(23,25).
pos(24,25).
pos(25,25).
pos(26,25).
pos(27,25).
pos(28,25).
pos(29,25).
pos(30,25).
pos(31,25).
pos(32,25).
pos(33,25).
pos(35,25).
pos(37,25).
pos(38,25).
pos(39,25).
pos(40,25).
pos(41,25).
pos(2,26).
pos(4,26).
pos(6,26).
pos(10,26).
pos(11,26).
pos(12,26).
pos(13,26).
pos(14,26).
pos(16,26).
pos(22,26).
pos(26,26).
pos(33,26).
pos(35,26).
pos(37,26).
pos(41,26).
pos(2,27).
pos(3,27).
pos(4,27).
pos(6,27).
pos(7,27).
pos(8,27).
pos(9,27).
pos(10,27).
pos(16,27).
pos(17,27).
pos(18,27).
pos(19,27).
pos(20,27).
pos(22,27).
pos(23,27).
pos(25,27).
pos(26,27).
pos(27,27).
pos(28,27).
pos(29,27).
pos(30,27).
pos(31,27).
pos(32,27).
pos(33,27).
pos(35,27).
pos(37,27).
pos(39,27).
pos(41,27).
pos(2,28).
pos(10,28).
pos(11,28).
pos(12,28).
pos(13,28).
pos(14,28).
pos(16,28).
pos(17,28).
pos(18,28).
pos(19,28).
pos(20,28).
pos(22,28).
pos(23,28).
pos(25,28).
pos(26,28).
pos(27,28).
pos(29,28).
pos(30,28).
pos(31,28).
pos(33,28).
pos(35,28).
pos(37,28).
pos(39,28).
pos(41,28).
pos(2,29).
pos(3,29).
pos(4,29).
pos(5,29).
pos(6,29).
pos(7,29).
pos(8,29).
pos(10,29).
pos(14,29).
pos(16,29).
pos(17,29).
pos(18,29).
pos(19,29).
pos(20,29).
pos(22,29).
pos(30,29).
pos(35,29).
pos(37,29).
pos(38,29).
pos(39,29).
pos(41,29).
pos(2,30).
pos(4,30).
pos(5,30).
pos(6,30).
pos(7,30).
pos(8,30).
pos(10,30).
pos(11,30).
pos(12,30).
pos(14,30).
pos(15,30).
pos(16,30).
pos(17,30).
pos(18,30).
pos(19,30).
pos(20,30).
pos(22,30).
pos(23,30).
pos(24,30).
pos(25,30).
pos(26,30).
pos(27,30).
pos(28,30).
pos(29,30).
pos(30,30).
pos(31,30).
pos(33,30).
pos(34,30).
pos(35,30).
pos(41,30).
pos(8,31).
pos(10,31).
pos(11,31).
pos(12,31).
pos(14,31).
pos(16,31).
pos(17,31).
pos(18,31).
pos(19,31).
pos(20,31).
pos(22,31).
pos(28,31).
pos(33,31).
pos(34,31).
pos(35,31).
pos(36,31).
pos(37,31).
pos(38,31).
pos(39,31).
pos(40,31).
pos(41,31).
pos(2,32).
pos(4,32).
pos(5,32).
pos(6,32).
pos(7,32).
pos(8,32).
pos(10,32).
pos(11,32).
pos(12,32).
pos(14,32).
pos(18,32).
pos(22,32).
pos(24,32).
pos(25,32).
pos(26,32).
pos(27,32).
pos(28,32).
pos(29,32).
pos(30,32).
pos(31,32).
pos(33,32).
pos(39,32).
pos(2,33).
pos(4,33).
pos(5,33).
pos(6,33).
pos(7,33).
pos(8,33).
pos(10,33).
pos(14,33).
pos(16,33).
pos(17,33).
pos(18,33).
pos(19,33).
pos(20,33).
pos(21,33).
pos(22,33).
pos(24,33).
pos(33,33).
pos(35,33).
pos(36,33).
pos(37,33).
pos(38,33).
pos(39,33).
pos(41,33).
pos(2,34).
pos(4,34).
pos(5,34).
pos(6,34).
pos(7,34).
pos(8,34).
pos(9,34).
pos(10,34).
pos(11,34).
pos(12,34).
pos(13,34).
pos(14,34).
pos(16,34).
pos(17,34).
pos(19,34).
pos(20,34).
pos(21,34).
pos(22,34).
pos(24,34).
pos(25,34).
pos(27,34).
pos(28,34).
pos(29,34).
pos(30,34).
pos(31,34).
pos(32,34).
pos(33,34).
pos(35,34).
pos(36,34).
pos(38,34).
pos(39,34).
pos(41,34).
pos(2,35).
pos(13,35).
pos(16,35).
pos(21,35).
pos(25,35).
pos(27,35).
pos(35,35).
pos(36,35).
pos(38,35).
pos(39,35).
pos(41,35).
pos(2,36).
pos(3,36).
pos(4,36).
pos(5,36).
pos(6,36).
pos(7,36).
pos(8,36).
pos(9,36).
pos(10,36).
pos(11,36).
pos(12,36).
pos(13,36).
pos(14,36).
pos(15,36).
pos(16,36).
pos(17,36).
pos(19,36).
pos(20,36).
pos(21,36).
pos(22,36).
pos(23,36).
pos(25,36).
pos(27,36).
pos(29,36).
pos(30,36).
pos(31,36).
pos(32,36).
pos(33,36).
pos(34,36).
pos(35,36).
pos(36,36).
pos(38,36).
pos(39,36).
pos(41,36).
pos(10,37).
pos(17,37).
pos(19,37).
pos(20,37).
pos(21,37).
pos(22,37).
pos(23,37).
pos(25,37).
pos(27,37).
pos(29,37).
pos(30,37).
pos(32,37).
pos(33,37).
pos(34,37).
pos(35,37).
pos(36,37).
pos(37,37).
pos(38,37).
pos(39,37).
pos(41,37).
pos(2,38).
pos(3,38).
pos(4,38).
pos(5,38).
pos(6,38).
pos(7,38).
pos(8,38).
pos(10,38).
pos(12,38).
pos(13,38).
pos(15,38).
pos(17,38).
pos(19,38).
pos(20,38).
pos(21,38).
pos(22,38).
pos(23,38).
pos(25,38).
pos(27,38).
pos(29,38).
pos(30,38).
pos(36,38).
pos(41,38).
pos(2,39).
pos(4,39).
pos(5,39).
pos(6,39).
pos(7,39).
pos(8,39).
pos(10,39).
pos(12,39).
pos(13,39).
pos(15,39).
pos(17,39).
pos(18,39).
pos(19,39).
pos(20,39).
pos(21,39).
pos(22,39).
pos(23,39).
pos(24,39).
pos(25,39).
pos(26,39).
pos(27,39).
pos(29,39).
pos(30,39).
pos(32,39).
pos(33,39).
pos(34,39).
pos(35,39).
pos(36,39).
pos(37,39).
pos(38,39).
pos(39,39).
pos(40,39).
pos(41,39).
pos(2,40).
pos(10,40).
pos(12,40).
pos(13,40).
pos(15,40).
pos(27,40).
pos(29,40).
pos(41,40).
pos(2,41).
pos(3,41).
pos(4,41).
pos(5,41).
pos(6,41).
pos(7,41).
pos(8,41).
pos(9,41).
pos(10,41).
pos(11,41).
pos(12,41).
pos(13,41).
pos(14,41).
pos(15,41).
pos(16,41).
pos(17,41).
pos(18,41).
pos(19,41).
pos(20,41).
pos(21,41).
pos(22,41).
pos(23,41).
pos(24,41).
pos(25,41).
pos(26,41).
pos(27,41).
pos(28,41).
pos(29,41).
pos(30,41).
pos(31,41).
pos(32,41).
pos(33,41).
pos(34,41).
pos(36,41).
pos(37,41).
pos(38,41).
pos(39,41).
pos(40,41).
pos(41,41).

at(breeze,pos(10,21)).
at(breeze,pos(11,22)).
at(breeze,pos(12,10)).
at(breeze,pos(13,11)).
at(breeze,pos(14,10)).
at(breeze,pos(16,28)).
at(breeze,pos(17,27)).
at(breeze,pos(17,29)).
at(breeze,pos(18,28)).
at(breeze,pos(18,29)).
at(breeze,pos(19,28)).
at(breeze,pos(19,30)).
at(breeze,pos(2,9)).
at(breeze,pos(20,29)).
at(breeze,pos(22,26)).
at(breeze,pos(23,25)).
at(breeze,pos(3,15)).
at(breeze,pos(30,23)).
at(breeze,pos(32,23)).
at(breeze,pos(35,27)).
at(breeze,pos(35,29)).
at(breeze,pos(4,9)).
at(breeze,pos(5,15)).
at(breeze,pos(6,17)).
at(breeze,pos(8,17)).
at(fairies,pos(10,5)).
at(fairies,pos(12,8)).
at(fairies,pos(17,4)).
at(fairies,pos(18,2)).
at(fairies,pos(19,12)).
at(fairies,pos(2,8)).
at(fairies,pos(22,30)).
at(fairies,pos(22,9)).
at(fairies,pos(24,10)).
at(fairies,pos(24,16)).
at(fairies,pos(24,19)).
at(fairies,pos(24,34)).
at(fairies,pos(24,39)).
at(fairies,pos(25,25)).
at(fairies,pos(25,41)).
at(fairies,pos(28,6)).
at(fairies,pos(3,15)).
at(fairies,pos(31,6)).
at(fairies,pos(31,8)).
at(fairies,pos(32,16)).
at(fairies,pos(32,2)).
at(fairies,pos(33,6)).
at(fairies,pos(35,26)).
at(fairies,pos(35,7)).
at(fairies,pos(37,21)).
at(fairies,pos(41,3)).
at(fairies,pos(41,9)).
at(fairies,pos(5,39)).
at(fairies,pos(6,39)).
at(fairies,pos(8,9)).
at(hole,pos(10,22)).
at(hole,pos(13,10)).
at(hole,pos(17,28)).
at(hole,pos(19,29)).
at(hole,pos(22,25)).
at(hole,pos(3,9)).
at(hole,pos(31,23)).
at(hole,pos(35,28)).
at(hole,pos(4,15)).
at(hole,pos(7,17)).
at(master_sword,pos(38,5)).
at(monster,pos(10,30)).
at(monster,pos(10,38)).
at(monster,pos(10,40)).
at(monster,pos(11,20)).
at(monster,pos(11,34)).
at(monster,pos(12,22)).
at(monster,pos(12,24)).
at(monster,pos(12,39)).
at(monster,pos(13,38)).
at(monster,pos(14,10)).
at(monster,pos(14,34)).
at(monster,pos(15,19)).
at(monster,pos(16,22)).
at(monster,pos(16,23)).
at(monster,pos(16,33)).
at(monster,pos(17,11)).
at(monster,pos(17,37)).
at(monster,pos(17,38)).
at(monster,pos(18,4)).
at(monster,pos(19,20)).
at(monster,pos(19,37)).
at(monster,pos(2,15)).
at(monster,pos(2,28)).
at(monster,pos(2,29)).
at(monster,pos(2,32)).
at(monster,pos(2,38)).
at(monster,pos(20,10)).
at(monster,pos(20,36)).
at(monster,pos(22,13)).
at(monster,pos(22,17)).
at(monster,pos(22,2)).
at(monster,pos(23,28)).
at(monster,pos(24,15)).
at(monster,pos(24,32)).
at(monster,pos(24,41)).
at(monster,pos(25,17)).
at(monster,pos(25,25)).
at(monster,pos(25,27)).
at(monster,pos(25,35)).
at(monster,pos(26,12)).
at(monster,pos(26,14)).
at(monster,pos(26,18)).
at(monster,pos(26,2)).
at(monster,pos(26,8)).
at(monster,pos(27,21)).
at(monster,pos(27,3)).
at(monster,pos(27,5)).
at(monster,pos(28,14)).
at(monster,pos(28,19)).
at(monster,pos(28,20)).
at(monster,pos(28,32)).
at(monster,pos(29,12)).
at(monster,pos(29,3)).
at(monster,pos(29,4)).
at(monster,pos(3,36)).
at(monster,pos(30,39)).
at(monster,pos(31,3)).
at(monster,pos(31,4)).
at(monster,pos(32,2)).
at(monster,pos(32,23)).
at(monster,pos(32,25)).
at(monster,pos(32,41)).
at(monster,pos(33,11)).
at(monster,pos(33,2)).
at(monster,pos(33,21)).
at(monster,pos(35,13)).
at(monster,pos(35,37)).
at(monster,pos(35,7)).
at(monster,pos(36,15)).
at(monster,pos(36,36)).
at(monster,pos(36,38)).
at(monster,pos(36,4)).
at(monster,pos(37,11)).
at(monster,pos(37,21)).
at(monster,pos(37,25)).
at(monster,pos(37,37)).
at(monster,pos(37,6)).
at(monster,pos(37,9)).
at(monster,pos(38,15)).
at(monster,pos(38,29)).
at(monster,pos(39,17)).
at(monster,pos(39,31)).
at(monster,pos(4,19)).
at(monster,pos(4,20)).
at(monster,pos(4,39)).
at(monster,pos(4,4)).
at(monster,pos(4,5)).
at(monster,pos(41,11)).
at(monster,pos(6,13)).
at(monster,pos(6,19)).
at(monster,pos(6,2)).
at(monster,pos(6,22)).
at(monster,pos(6,26)).
at(monster,pos(6,29)).
at(monster,pos(6,33)).
at(monster,pos(6,34)).
at(monster,pos(6,9)).
at(monster,pos(8,14)).
at(monster,pos(8,29)).
at(monster,pos(8,30)).
at(noises,pos(10,20)).
at(noises,pos(10,29)).
at(noises,pos(10,31)).
at(noises,pos(10,34)).
at(noises,pos(10,37)).
at(noises,pos(10,39)).
at(noises,pos(10,41)).
at(noises,pos(11,22)).
at(noises,pos(11,24)).
at(noises,pos(11,30)).
at(noises,pos(12,20)).
at(noises,pos(12,34)).
at(noises,pos(12,38)).
at(noises,pos(12,40)).
at(noises,pos(13,10)).
at(noises,pos(13,22)).
at(noises,pos(13,24)).
at(noises,pos(13,34)).
at(noises,pos(13,39)).
at(noises,pos(14,11)).
at(noises,pos(14,33)).
at(noises,pos(15,10)).
at(noises,pos(15,18)).
at(noises,pos(15,20)).
at(noises,pos(16,22)).
at(noises,pos(16,23)).
at(noises,pos(16,24)).
at(noises,pos(16,34)).
at(noises,pos(17,10)).
at(noises,pos(17,12)).
at(noises,pos(17,22)).
at(noises,pos(17,23)).
at(noises,pos(17,33)).
at(noises,pos(17,36)).
at(noises,pos(17,37)).
at(noises,pos(17,38)).
at(noises,pos(17,39)).
at(noises,pos(17,4)).
at(noises,pos(18,20)).
at(noises,pos(19,10)).
at(noises,pos(19,19)).
at(noises,pos(19,36)).
at(noises,pos(19,38)).
at(noises,pos(2,14)).
at(noises,pos(2,27)).
at(noises,pos(2,28)).
at(noises,pos(2,29)).
at(noises,pos(2,30)).
at(noises,pos(2,33)).
at(noises,pos(2,36)).
at(noises,pos(2,39)).
at(noises,pos(20,11)).
at(noises,pos(20,20)).
at(noises,pos(20,37)).
at(noises,pos(20,9)).
at(noises,pos(21,13)).
at(noises,pos(21,2)).
at(noises,pos(21,36)).
at(noises,pos(22,12)).
at(noises,pos(22,16)).
at(noises,pos(22,18)).
at(noises,pos(22,28)).
at(noises,pos(23,15)).
at(noises,pos(23,2)).
at(noises,pos(23,27)).
at(noises,pos(23,41)).
at(noises,pos(24,14)).
at(noises,pos(24,16)).
at(noises,pos(24,17)).
at(noises,pos(24,25)).
at(noises,pos(24,33)).
at(noises,pos(25,2)).
at(noises,pos(25,28)).
at(noises,pos(25,32)).
at(noises,pos(25,34)).
at(noises,pos(25,36)).
at(noises,pos(25,41)).
at(noises,pos(25,8)).
at(noises,pos(26,13)).
at(noises,pos(26,15)).
at(noises,pos(26,17)).
at(noises,pos(26,19)).
at(noises,pos(26,21)).
at(noises,pos(26,25)).
at(noises,pos(26,27)).
at(noises,pos(27,12)).
at(noises,pos(27,14)).
at(noises,pos(27,18)).
at(noises,pos(27,19)).
at(noises,pos(27,2)).
at(noises,pos(27,32)).
at(noises,pos(27,4)).
at(noises,pos(27,6)).
at(noises,pos(27,8)).
at(noises,pos(28,12)).
at(noises,pos(28,19)).
at(noises,pos(28,20)).
at(noises,pos(28,21)).
at(noises,pos(28,31)).
at(noises,pos(29,14)).
at(noises,pos(29,19)).
at(noises,pos(29,2)).
at(noises,pos(29,3)).
at(noises,pos(29,32)).
at(noises,pos(29,39)).
at(noises,pos(29,4)).
at(noises,pos(29,5)).
at(noises,pos(3,15)).
at(noises,pos(3,29)).
at(noises,pos(3,38)).
at(noises,pos(30,12)).
at(noises,pos(30,38)).
at(noises,pos(30,4)).
at(noises,pos(31,2)).
at(noises,pos(31,23)).
at(noises,pos(31,25)).
at(noises,pos(31,3)).
at(noises,pos(31,4)).
at(noises,pos(31,41)).
at(noises,pos(32,11)).
at(noises,pos(32,2)).
at(noises,pos(32,21)).
at(noises,pos(32,4)).
at(noises,pos(33,10)).
at(noises,pos(33,12)).
at(noises,pos(33,2)).
at(noises,pos(33,20)).
at(noises,pos(33,22)).
at(noises,pos(33,23)).
at(noises,pos(33,25)).
at(noises,pos(33,41)).
at(noises,pos(34,13)).
at(noises,pos(34,2)).
at(noises,pos(34,37)).
at(noises,pos(35,12)).
at(noises,pos(35,14)).
at(noises,pos(35,15)).
at(noises,pos(35,36)).
at(noises,pos(35,4)).
at(noises,pos(35,6)).
at(noises,pos(36,21)).
at(noises,pos(36,35)).
at(noises,pos(36,37)).
at(noises,pos(36,39)).
at(noises,pos(36,5)).
at(noises,pos(36,6)).
at(noises,pos(36,7)).
at(noises,pos(36,9)).
at(noises,pos(37,12)).
at(noises,pos(37,15)).
at(noises,pos(37,26)).
at(noises,pos(37,29)).
at(noises,pos(37,4)).
at(noises,pos(37,5)).
at(noises,pos(37,7)).
at(noises,pos(38,11)).
at(noises,pos(38,17)).
at(noises,pos(38,21)).
at(noises,pos(38,25)).
at(noises,pos(38,31)).
at(noises,pos(38,37)).
at(noises,pos(38,6)).
at(noises,pos(38,9)).
at(noises,pos(39,15)).
at(noises,pos(39,29)).
at(noises,pos(39,32)).
at(noises,pos(4,19)).
at(noises,pos(4,20)).
at(noises,pos(4,21)).
at(noises,pos(4,36)).
at(noises,pos(4,38)).
at(noises,pos(4,4)).
at(noises,pos(4,5)).
at(noises,pos(4,6)).
at(noises,pos(40,11)).
at(noises,pos(40,31)).
at(noises,pos(41,12)).
at(noises,pos(5,13)).
at(noises,pos(5,19)).
at(noises,pos(5,2)).
at(noises,pos(5,29)).
at(noises,pos(5,33)).
at(noises,pos(5,34)).
at(noises,pos(5,39)).
at(noises,pos(5,4)).
at(noises,pos(6,10)).
at(noises,pos(6,12)).
at(noises,pos(6,20)).
at(noises,pos(6,23)).
at(noises,pos(6,25)).
at(noises,pos(6,27)).
at(noises,pos(6,30)).
at(noises,pos(6,32)).
at(noises,pos(6,33)).
at(noises,pos(6,34)).
at(noises,pos(6,8)).
at(noises,pos(7,19)).
at(noises,pos(7,2)).
at(noises,pos(7,22)).
at(noises,pos(7,29)).
at(noises,pos(7,30)).
at(noises,pos(7,33)).
at(noises,pos(7,34)).
at(noises,pos(8,13)).
at(noises,pos(8,15)).
at(noises,pos(8,29)).
at(noises,pos(8,30)).
at(noises,pos(8,31)).
at(pendants_glow,pos(10,32)).
at(pendants_glow,pos(12,11)).
at(pendants_glow,pos(12,22)).
at(pendants_glow,pos(12,8)).
at(pendants_glow,pos(16,35)).
at(pendants_glow,pos(18,30)).
at(pendants_glow,pos(19,37)).
at(pendants_glow,pos(20,27)).
at(pendants_glow,pos(22,11)).
at(pendants_glow,pos(23,6)).
at(pendants_glow,pos(24,34)).
at(pendants_glow,pos(27,12)).
at(pendants_glow,pos(27,21)).
at(pendants_glow,pos(28,27)).
at(pendants_glow,pos(3,27)).
at(pendants_glow,pos(30,30)).
at(pendants_glow,pos(31,23)).
at(pendants_glow,pos(33,33)).
at(pendants_glow,pos(33,6)).
at(pendants_glow,pos(34,36)).
at(pendants_glow,pos(36,33)).
at(pendants_glow,pos(36,39)).
at(pendants_glow,pos(37,9)).
at(pendants_glow,pos(38,5)).
at(pendants_glow,pos(39,39)).
at(pendants_glow,pos(4,26)).
at(pendants_glow,pos(5,19)).
at(pendants_glow,pos(8,10)).
at(pendants_glow,pos(8,25)).
at(pendants_glow,pos(8,38)).
at(pendants_glow,pos(9,36)).
at(rupee_glow,pos(10,22)).
at(rupee_glow,pos(10,29)).
at(rupee_glow,pos(10,41)).
at(rupee_glow,pos(12,11)).
at(rupee_glow,pos(13,24)).
at(rupee_glow,pos(14,30)).
at(rupee_glow,pos(15,4)).
at(rupee_glow,pos(17,14)).
at(rupee_glow,pos(17,15)).
at(rupee_glow,pos(17,22)).
at(rupee_glow,pos(17,23)).
at(rupee_glow,pos(19,13)).
at(rupee_glow,pos(2,11)).
at(rupee_glow,pos(2,19)).
at(rupee_glow,pos(2,20)).
at(rupee_glow,pos(2,21)).
at(rupee_glow,pos(2,28)).
at(rupee_glow,pos(20,11)).
at(rupee_glow,pos(21,2)).
at(rupee_glow,pos(24,14)).
at(rupee_glow,pos(24,34)).
at(rupee_glow,pos(25,21)).
at(rupee_glow,pos(25,30)).
at(rupee_glow,pos(26,17)).
at(rupee_glow,pos(26,21)).
at(rupee_glow,pos(26,4)).
at(rupee_glow,pos(26,41)).
at(rupee_glow,pos(27,32)).
at(rupee_glow,pos(28,12)).
at(rupee_glow,pos(29,12)).
at(rupee_glow,pos(29,32)).
at(rupee_glow,pos(29,34)).
at(rupee_glow,pos(30,25)).
at(rupee_glow,pos(30,39)).
at(rupee_glow,pos(30,6)).
at(rupee_glow,pos(31,32)).
at(rupee_glow,pos(32,27)).
at(rupee_glow,pos(35,11)).
at(rupee_glow,pos(35,5)).
at(rupee_glow,pos(37,2)).
at(rupee_glow,pos(37,6)).
at(rupee_glow,pos(38,2)).
at(rupee_glow,pos(39,27)).
at(rupee_glow,pos(39,28)).
at(rupee_glow,pos(41,23)).
at(rupee_glow,pos(5,15)).
at(rupee_glow,pos(6,26)).
at(rupee_glow,pos(8,33)).
at(rupee_glow,pos(8,8)).
at(rupee_glow,pos(8,9)).
at(spatial_distortions,pos(13,11)).
at(spatial_distortions,pos(14,10)).
at(spatial_distortions,pos(15,11)).
at(spatial_distortions,pos(19,33)).
at(spatial_distortions,pos(20,34)).
at(spatial_distortions,pos(21,33)).
at(spatial_distortions,pos(22,21)).
at(spatial_distortions,pos(22,32)).
at(spatial_distortions,pos(22,34)).
at(spatial_distortions,pos(24,21)).
at(spatial_distortions,pos(25,21)).
at(spatial_distortions,pos(25,27)).
at(spatial_distortions,pos(26,26)).
at(spatial_distortions,pos(26,28)).
at(spatial_distortions,pos(27,21)).
at(spatial_distortions,pos(27,27)).
at(spatial_distortions,pos(28,27)).
at(spatial_distortions,pos(29,28)).
at(spatial_distortions,pos(30,27)).
at(spatial_distortions,pos(35,14)).
at(spatial_distortions,pos(35,16)).
at(spatial_distortions,pos(36,15)).
at(spatial_distortions,pos(39,39)).
at(spatial_distortions,pos(4,13)).
at(spatial_distortions,pos(41,39)).
at(spatial_distortions,pos(6,13)).
at(vortex,pos(14,11)).
at(vortex,pos(19,34)).
at(vortex,pos(22,33)).
at(vortex,pos(23,21)).
at(vortex,pos(26,21)).
at(vortex,pos(26,27)).
at(vortex,pos(29,27)).
at(vortex,pos(35,15)).
at(vortex,pos(40,39)).
at(vortex,pos(5,13)).

% ------------------------------------------------------------------------------
%
%   INITIAL PROBLEM CONFIG
%
% ------------------------------------------------------------------------------

	% ----------------------------------------------------------------------------
	%  Definition of the agent.
	% ----------------------------------------------------------------------------

		at( agent , pos(21 , 38) ). % Agent starts here %

	% ----------------------------------------------------------------------------
	%  Valid items.
	% ----------------------------------------------------------------------------

		item( rupee ).
		item( heart ).
		item( sword ).

	% ----------------------------------------------------------------------------
	%  Initial state of the agent.
	% ----------------------------------------------------------------------------

		safe( pos( 21 , 38) ).
		visited( pos(21 , 38) ).		
		energy( agent , 100 ).


