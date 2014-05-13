% ------------------------------------------------------------------------------
%
%   MODULE - **actions** ( alias: actions )
%
% ------------------------------------------------------------------------------
	
	:- module( actions , [ 
		best_action/1,
		move/2,
		pickup_item/3,
		attack_monster/2,
		on_vortex/0,
		get_random_position/2,
		add_energy/1,
		sub_energy/1
	]).

	:- use_module( problem ).

% ------------------------------------------------------------------------------
%
%   EXPORTED PREDICATES
%
% ------------------------------------------------------------------------------

	% ----------------------------------------------------------------------------
	%  Description
	%    The agent has died, hence there`s no possible action.
	%
	%  OBS: Death code reference: https://vine.co/v/b3XZMHmxzxh 
	% ----------------------------------------------------------------------------
		
		best_action( like_omg_dead ) :- 
			is_dead.

	% ----------------------------------------------------------------------------
	%  Description
	%    The agent has obtained the real master sword. It won!
	% ----------------------------------------------------------------------------
		
		best_action( won ) :- 
			has_won.

	% ----------------------------------------------------------------------------
	%  Description
	%    The 2nd best action is to pickup an item (except hearts), if one exists 
	%		   where the player is currently at.
	% ----------------------------------------------------------------------------

		best_action( pickup_item( Item , X , Y ) ) :-
			at( agent , pos(X , Y) ) ,
			item( Item ),
				at( Item , pos(X , Y) ),
				Item \= heart.

	% ----------------------------------------------------------------------------
	%  Description
	%    The 3rd best action is to pickup a heart, if one exists where the 
	%		   player is currently at.
	% ----------------------------------------------------------------------------

		best_action( pickup_item( heart , X , Y ) ) :-
			at( agent , pos(X , Y) ) ,
			at( heart , pos(X , Y) ) ,
			energy( agent , Energy ) , 
				Energy =< 50.

	
	% ----------------------------------------------------------------------------
	%  Description
	%    The 4th best action is to move to random valid position, if the agent 
	%		   find itself on a vortex.
	% ----------------------------------------------------------------------------

		best_action( move( ToX , ToY ) ) :-
			on_vortex,
			pos( ToX , ToY ),
			at( agent , pos(FromX,FromY) ),
			check_complete_sensing(FromX,FromY),
			get_random_position( pos(FromX,FromY) , pos(ToX,ToY) ).


	% ----------------------------------------------------------------------------
	%  Description
	%    The 5th best action is to move to a heart, if the agent`s energy is so 
	%		   low (=<10) that it actually makes sense to go there.
	% ----------------------------------------------------------------------------

		best_action( move( ToX , ToY ) ) :-
			pos( ToX , ToY ),
			energy( agent , Energy ) , 
				Energy =< 10,
			at( agent , pos(FromX , FromY) ),
			at( heart , pos(ToX , ToY) ),
				can_move( FromX , FromY , ToX , ToY),
				check_complete_sensing(FromX,FromY),
				safe( pos(ToX , ToY) ).

	% ----------------------------------------------------------------------------
	%  Description
	%    The 6th best action is to move to a not yet visited position 
	%			 known to be safe by the agent, preferably adjacent.
	% ----------------------------------------------------------------------------

		best_action( move( ToX , ToY ) ) :-
			at( agent , pos(FromX , FromY) ),
				check_complete_sensing(FromX,FromY),
			pos( ToX, ToY ),
				safe( pos(ToX , ToY) ),
				not( visited( pos(ToX, ToY) )),
				is_adjacent(pos( ToX , ToY ), pos(FromX , FromY)),
				can_move( FromX , FromY , ToX , ToY).

	% ----------------------------------------------------------------------------
	%  Description
	%    The 7th best action is to move to a not yet visited position 
	%			 known to be safe by the agent, now far away from where it is.
	% ----------------------------------------------------------------------------

		best_action( move( ToX , ToY ) ) :-
			at( agent , pos(FromX , FromY) ),
				check_complete_sensing(FromX,FromY),
			pos( ToX , ToY ),
				safe( pos(ToX , ToY) ),
				not( visited( pos(ToX, ToY) )),
				can_move( FromX , FromY , ToX , ToY).

	% ----------------------------------------------------------------------------
	%  Description
	%    The second-to-last best action is to attack a monster at an adjacent 
	%			 position. But only if there`s such a monster and the agent has 
	%			 enough energy.
	% ----------------------------------------------------------------------------

		best_action( attack_monster( X , Y ) ) :-
			energy( agent , Energy ) , 
				Energy >= 11,
			at( agent , pos(Xg , Yg) ),
				check_complete_sensing(Xg,Yg),
			(at(actual_monster , pos(X,Y));
			 at(potential_monster , pos(X,Y))),
				is_adjacent(pos(Xg,Yg), pos(X,Y)),!.

	% ----------------------------------------------------------------------------
	%  Description
	%    The last best action is to move to a position with some danger in it, 
	%		   or in the case of a monster to a position adjacent to the monster.
	%    We never move to a position where there might be a hole. Try our luck at 
	%      some vortex instead.
	% ----------------------------------------------------------------------------

		% Move next to where we believe there`s a monster
			best_action( move( ToX , ToY ) ) :-
				at( agent , pos(FromX , FromY)),
				(at( actual_monster, pos(Mx,My)),!;
				at( potential_monster, pos(Mx,My)),!),
				pos( ToX , ToY ),
					is_adjacent( pos(ToX,ToY), pos(Mx,My)),
					visited( pos(ToX , ToY) ),!,
					can_move( FromX , FromY , ToX , ToY),
					check_complete_sensing(FromX,FromY).

		% Move to a vortex. gg
			best_action( move( Dx , Dy ) ) :-
				( at( potential_vortex, pos(Dx,Dy)),!;
					at( actual_vortex, pos(Dx,Dy)),!),
				at( agent , pos(FromX , FromY)),
					can_move( FromX , FromY , Dx , Dy),
					check_complete_sensing(FromX,FromY).

		% Move to some pending position
			best_action( move( ToX , ToY ) ) :-
				at( agent , pos(FromX , FromY)),
					check_complete_sensing(FromX,FromY),
				should_visit( pos(ToX,ToY) ),
				pos(ToX,ToY).


	% ----------------------------------------------------------------------------
	%  Description
	%    Moves the agent to user specified ToX and ToY if such movement is valid.
	% 
	%  Effects 
	%    - removes agent from its previous position (FromX, FromY)
	%    - asserts the new position (ToX,ToY) of the agent
	%    - mark the new position as visited to prevent possible fruitless future
	%			   movements
	% ----------------------------------------------------------------------------

		move( ToX , ToY ) :-
			at( agent, pos(FromX , FromY) ),
				retract( at( agent , pos(FromX , FromY) )),
				asserta( at( agent , pos(ToX, ToY) )),
			((
				not( visited(pos(ToX,ToY))),
				assertz( visited( pos(ToX , ToY) ))
			); true),
			((
				should_visit(pos( ToX , ToY)),
				retract( should_visit(pos( ToX , ToY)) )
			); true).

	% ----------------------------------------------------------------------------
	%  Description
	%    The agent picks up an item at (X,Y). Makes sure it actually 
	%		 exists before performing any action.
	%
	%  Effects
	%    - the agent picks up the item and then it is retracted from
	%			   the database.
	% ----------------------------------------------------------------------------

		pickup_item( heart , X , Y ) :-
			at( agent , pos(X , Y) ), 
			at( heart , pos(X , Y) ),
				retract( at( heart , pos(X , Y) )),
			add_energy( 50 ).

		pickup_item( Item , X , Y ) :-
			item( Item ),
			at( agent , pos(X , Y) ), 
			at( Item , pos(X , Y) ),
				retract( at( Item , pos(X , Y) )).

	% ----------------------------------------------------------------------------
	%  Description
	%    The agent attacks the monster at (X,Y), if it believes there`s a monster
	%		   there or, in the worst case, if it had some indication. 
	%
	%  Effects
	%    - The belief that there`s monster is deleted.
	%		 - If a monster actually exists there, it is deleted.
	%		 - The position where the monster was is now certainly safe.
	%    - The agent`s energy is decreased by 10 units.
	% ----------------------------------------------------------------------------
	
		% Strong belief of monster at (X,Y)	 
			attack_monster( X , Y ) :-
				at( actual_monster , pos(X,Y) ),
					retract( at( actual_monster, pos(X,Y) )),
					(retract( at( potential_monster, pos(X,Y) )); true),
					(( no_dangers(pos(X,Y)), asserta( safe( pos(X,Y))) );true),
					((at(monster,pos(X,Y)),
						retract( at( monster, pos(X,Y)))); 
					true),
				!,sub_energy( 10 ),!.

		% Weak belief of potential monster at (X,Y)
			attack_monster( X , Y ) :-
				at( potential_monster , pos(X,Y) ),
					retract( at( potential_monster, pos(X,Y) )),
					(retract( at( actual_monster, pos(X,Y) )); true),
					(( no_dangers(pos(X,Y)), asserta( safe( pos(X,Y))) );true),
					((at(monster,pos(X,Y)),
						retract( at( monster, pos(X,Y)))); 
					true),
				!,sub_energy( 10 ),!.

% ------------------------------------------------------------------------------
%
%   HELPERS
%
% ------------------------------------------------------------------------------

	% ----------------------------------------------------------------------------
	%  Description
	%    Checks if moving the agent to (ToX, ToY) is valid.
	% ----------------------------------------------------------------------------

		can_move( FromX , FromY , ToX , ToY ) :-
			pos( FromX , FromY ),
			pos( ToX , ToY ),
			not( (FromX == ToX , FromY == ToY) ).

	% ----------------------------------------------------------------------------
	%  Description
	%    ( Adds to | Subtracts from ) energy an amount of points equal to Value.
	% ----------------------------------------------------------------------------

		add_energy( Value ) :-
			energy( agent , Energy ) , 
				NewEnergy is min(Energy + Value, 100),
				asserta( energy(agent , NewEnergy)),
				retract( energy(agent, Energy)).

		sub_energy( Value ) :-
			energy( agent , Energy ) , 
				NewEnergy is max(Energy - Value, 0),
				asserta( energy(agent , NewEnergy)),
				retract( energy(agent, Energy)).

	% ----------------------------------------------------------------------------
	%  Description
	%    Checks if a position and its surroundings have been fully sensed.
	% ----------------------------------------------------------------------------
		
		check_complete_sensing( X , Y ) :-
			sensed( pos(X,Y) , local ),
			sensed( pos(X,Y) , around ).	

	% ----------------------------------------------------------------------------
	%  Description
	%    Get random pos(ToX,ToY).
	% ----------------------------------------------------------------------------
		
		
		get_random_position( Except , To ) :-
			random( 1, 43, X),
			random( 1, 43, Y),
			(pos(X,Y), pos(X,Y) \= Except, To = pos(X,Y), !);
			get_random_position( Except , To ).

		no_dangers( Pos ) :-
			not( at(actual_monster, Pos ) ),
			not( at(actual_vortex, Pos ) ),
			not( at(actual_hole, Pos ) ),
			not( at(potential_monster, Pos ) ),
			not( at(potential_vortex, Pos ) ),
			not( at(potential_hole, Pos ) ).

