% -----------------------------------------------------------------------------
%  Load actions and perception modules.
% -----------------------------------------------------------------------------

 :- module( p , [
 	  	at/2,
 	  	visited/1,
 	  	pos/2,
 	  	item/1,
 	  	safe/1,
 	  	energy/2
 	  ]).

	:- dynamic 
			at/2.
	:- dynamic
		visited/1.

% -----------------------------------------------------------------------------
%  Definitions of the structure of the map.
% -----------------------------------------------------------------------------

	pos( 20 , 38 ).
	pos( 20 , 40 ).
	pos( 20 , 39 ).
	pos( 21 , 38 ).
	pos( 21 , 40 ).
	pos( 21 , 39 ).
	pos( 22 , 38 ).
	pos( 22 , 40 ).
	pos( 22 , 39 ).

	at( agent , pos(21 , 38) ).
	at( rupee_glow , pos(21 , 40) ).
	at( fairies , pos(22, 39) ).
	at( pendants_glow , pos( 20 , 38 )).

	item( rupee ).
	item( heart ).
	item( sword ).

	safe( pos( 20 , 38 ) ).
	safe( pos( 20 , 40 ) ).
	safe( pos( 20 , 39 ) ).
	safe( pos( 21 , 38 ) ).
	safe( pos( 21 , 40 ) ).
	safe( pos( 21 , 39 ) ).
	safe( pos( 22 , 38 ) ).
	safe( pos( 22 , 40 ) ).
	safe( pos( 22 , 39 ) ).
	
	energy( agent , 100 ).

	visited( pos(21 , 38) ).


