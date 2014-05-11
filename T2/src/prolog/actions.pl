% ------------------------------------------------------------------------------
%
%   MODULE - **actions** ( alias: actions )
%
% ------------------------------------------------------------------------------
	
	:- module( actions , [ 
		best_action/1,
		move/2,
		pickup_item/3
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
	% ----------------------------------------------------------------------------

		% Death code reference: https://vine.co/v/b3XZMHmxzxh 
		best_action( like_omg_dead ) :-
			is_dead.

	% ----------------------------------------------------------------------------
	%  Description
	%    The best action is to pickup an item (except hearts), if one exists 
	%		   where the player is currently at.
	% ----------------------------------------------------------------------------

		best_action( pickup_item( Item , X , Y ) ) :-
			item( Item ),
			at( agent , pos(X , Y) ) ,
			at( Item , pos(X , Y) ).

	% ----------------------------------------------------------------------------
	%  Description
	%    The 2nd best action is to pickup a heart, if one exists where the 
	%		   player is currently at.
	% ----------------------------------------------------------------------------

		best_action( pickup_item( heart , X , Y ) ) :-
			at( agent , pos(X , Y) ) ,
			at( heart , pos(X , Y) ) ,
			energy( agent , Energy ) , 
			Energy < 50.

	% ----------------------------------------------------------------------------
	%  Description
	%    The 3rd best action is to move to a heart, if the agent`s energy is so 
	%		   low that it actually makes sense to go there.
	% ----------------------------------------------------------------------------

		best_action( move( ToX , ToY ) ) :-
			pos( ToX , ToY ),
			energy( agent , Energy ) , 
			Energy =< 10,
			at( agent , pos(FromX , FromY) ),
			at( heart , pos(ToX , ToY) ),
			can_move( FromX , FromY , ToX , ToY).

	% ----------------------------------------------------------------------------
	%  Description
	%    The last best action is to move to a not yet visited position known
	%		   to be safe by the agent.
	% ----------------------------------------------------------------------------

		best_action( move( ToX , ToY ) ) :-
			pos( ToX , ToY ),
			at( agent , pos(FromX , FromY) ),
			not( visited( pos(ToX, ToY) )),
			can_move( FromX , FromY , ToX , ToY).

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
			can_move( FromX , FromY , ToX , ToY ),
			retract( at( agent , pos(FromX , FromY) )),
			asserta( at( agent , pos(ToX, ToY) )),
			assertz( visited( pos(ToX , ToY) )).

	% ----------------------------------------------------------------------------
	%  Description
	%    The agent picks up an item at (X,Y). Makes sure it actually 
	%		 exists before performing any action.
	%
	%  Effects
	%    - the agent picks up the item and then it is retracted from
	%			   the database.
	% ----------------------------------------------------------------------------

		pickup_item( Item , X , Y ) :-
			item( Item ),
			at( agent , pos(X , Y) ), 
			at( Item , pos(X , Y) ),
			retract( at( Item , pos(X , Y) )).


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
			not( (FromX == ToX , FromY == ToY) ),
			safe( pos(ToX , ToY) ).
