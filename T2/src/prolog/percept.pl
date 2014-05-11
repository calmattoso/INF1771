% -----------------------------------------------------------------------------
%
%   SETUP MODULE
%
% -----------------------------------------------------------------------------
  
  :- module( percept , [percept/1] ).

  :- use_module( problem ).

% -----------------------------------------------------------------------------
%
%   EXPORTED PREDICATES
%
% -----------------------------------------------------------------------------

  percept( assertz( at(sword, pos(X , Y)))) :-
    at( agent , pos(X,Y) ),
    at( pendants_glow , pos(X,Y) ),
    retract( at( pendants_glow , pos(X,Y) )).

  percept( assertz( at(heart, pos(X , Y)))) :-
    at( agent , pos(X,Y) ),
    at( fairies , pos(X,Y) ),
    retract( at( fairies , pos(X,Y) ) ).  

  percept( assertz( at(rupee, pos(X, Y)))) :-
    at( agent , pos(X , Y) ),
    at( rupee_glow , pos(X , Y)),
    retract( at( rupee_glow , pos(X,Y) ) ).

% -----------------------------------------------------------------------------
%
%   HELPERS
%
% -----------------------------------------------------------------------------

