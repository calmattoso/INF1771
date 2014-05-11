% ------------------------------------------------------------------------------
%
%   MODULE - **percept** ( alias: percept )
%
% ------------------------------------------------------------------------------
  
  :- module( percept , [sense/1] ).

  :- use_module( problem ).


% ------------------------------------------------------------------------------
%
%   EXPORTED PREDICATES
%
% ------------------------------------------------------------------------------

  % ----------------------------------------------------------------------------
  %  Description
  %    Infers that there is a (genuine or fake) Master Sword at (X,Y).
  % 
  %  Effects 
  %    - Retracts the glow of pendants at (X,Y)
  %
  %  Returns
  %    - Assertion that exists a sword at position(X,Y).
  % ----------------------------------------------------------------------------
    
    sense( assertz( at(sword, pos(X , Y)))) :-
      at( agent , pos(X,Y) ),
      at( pendants_glow , pos(X,Y) ),
      retract( at( pendants_glow , pos(X,Y) )).

  % ----------------------------------------------------------------------------
  %  Description
  %    Infers that there`s a heart at postion (X,Y).
  % 
  %  Effects 
  %    - Retracts the fairies at (X,Y).
  %
  %  Returns
  %    - Assertion that exists a heart at position(X,Y).
  % ----------------------------------------------------------------------------

    sense( assertz( at(heart, pos(X , Y)))) :-
      at( agent , pos(X,Y) ),
      at( fairies , pos(X,Y) ),
      retract( at( fairies , pos(X,Y) ) ).  

  % ----------------------------------------------------------------------------
  %  Description
  %    Infers that there are rupees at postion (X,Y).
  % 
  %  Effects 
  %    - Retracts the glow of rupees at (X,Y).
  %
  %  Returns
  %    - Assertion that exists a rupee at position(X,Y).
  % ----------------------------------------------------------------------------

    sense( assertz( at(rupee, pos(X, Y)))) :-
      at( agent , pos(X , Y) ),
      at( rupee_glow , pos(X , Y)),
      retract( at( rupee_glow , pos(X,Y) ) ).

  % ----------------------------------------------------------------------------
  %  Description
  %    Infers the adjacent positions are safe, if the current one has no danger
  %      alarms in it. 
  %
  %  Returns
  %    - Command that sets adjacent positions to safe.
  % ----------------------------------------------------------------------------

    sense( adjust_safe ) :-
      at( agent , Pos ),
      not( at( spatial_distortions , Pos )),
      not( at( noises , Pos )),
      not( at( breeze , Pos )).

% ------------------------------------------------------------------------------
%
%   HELPERS
%
% ------------------------------------------------------------------------------

