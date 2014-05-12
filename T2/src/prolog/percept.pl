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
  %    Infers the agent has lost all energy for:
  %      1. Being in the same position of a monster.
  %      2. Being in the same position of a hole.
  %    Game`s over.  
  %
  %  Effects 
  %    - Retracts the previous energy of the agent.
  %
  %  Returns
  %    - Command that sets the new energy of the agent.
  % ----------------------------------------------------------------------------

    sense( asserta( energy(agent, 0)) ) :-
      at( agent ,   Pos ),
      (at(monster , Pos); at(hole, Pos)),
        retract( energy(agent,_) ).

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
  %    Infers the agent has picked up the real Master Sword, so the game`s over.
  %
  %  Returns
  %    - Command that informs the game`s been won.
  % ----------------------------------------------------------------------------

    sense( asserta( has_won ) ) :-
      at(agent ,   Pos),
      at(master_sword , Pos),
      not(at(sword,Pos)).

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
  %    Infers the agent is on a vortex.
  % 
  %  Effects 
  %    - Asserts the agent is on a vortex.
  %
  %  Returns
  %    - Command that sets the agent to be on a vortex.
  % ----------------------------------------------------------------------------

    sense( asserta( on_vortex )) :-
      not( on_vortex ),
      at( agent , Pos ),
      at( vortex, Pos ).

    sense( retract( on_vortex )) :-
      on_vortex,
      at( agent , Pos ),
      not( at( vortex, Pos ) ).

  % ----------------------------------------------------------------------------
  %  Description
  %    Makes sure the agent updates its knowledge about the current position.
  % 
  %  Effects 
  %    - Asserts the agent is on a vortex.
  %
  %  Returns
  %    - Command updates agent`s knowledge on its current position.
  % ----------------------------------------------------------------------------

    sense( check_local ) :-
      not( sensed( Pos , local)),
        asserta( sensed( Pos , local) ).


  % ----------------------------------------------------------------------------
  %  Description
  %    Infers the adjacent positions are safe, if the current one has no danger
  %      alarms in it. Happens once.
  % 
  %  Effects 
  %    - Asserts the agent has ascertained safety of its surroundings.
  %
  %  Returns
  %    - Command that sets adjacent positions to safe.
  % ----------------------------------------------------------------------------

    sense( adjust_safe ) :-
      at( agent , Pos ),
        not( sensed( Pos , around)),
        not( at( spatial_distortions , Pos )),
        not( at( noises , Pos )),
        not( at( breeze , Pos )),
          asserta( sensed( Pos , around)).

  % ----------------------------------------------------------------------------
  %  Description
  %    Infers the adjacent positions are potentially unsafe, if the current one 
  %      has at least one danger warning. Happens once.
  % 
  %  Effects 
  %    - Asserts the agent has ascertained possible unsafety of its
  %        surroundings.
  %
  %  Returns
  %    - Command that might set adjacent positions to potentially unsafe.
  % ----------------------------------------------------------------------------

    sense( mark_unsafe ) :-
      at( agent , Pos ),
      not( sensed( Pos , around )),
      ( at( spatial_distortions , Pos );
        at( noises , Pos );
        at( breeze , Pos ) ),
      asserta( sensed( Pos , around ) ).

% ------------------------------------------------------------------------------
%
%   HELPERS
%
% ------------------------------------------------------------------------------
