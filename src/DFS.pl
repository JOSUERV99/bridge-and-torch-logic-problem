/*
Problem:
    Some  friends are walking back to their hostel and
    they  come across a dilapidated old wooden bridge.
    The  bridge  is  very  weak  and  is  only able to
    support the weight of some of them at a time. They
    are  in  a  hurry  since  it is dusk and they must
    cross  in the shortest possible time. They carry a
    flashlight  that allows those who are crossing the
    bridge to see, but it does not serve to illuminate
    the  entire  bridge.  So if several of them cross,
    one   of   the   crossers  must  return  with  the
    flashlight to allow the others to cross.

    Because  they  all  have different fitness levels,
    and  some  are  injured,  it  takes each of them a
    different time to cross the bridge.
*/

:-include('Utils.pl').

% problem params
people([ ('Alberto',1),('Beatriz',2),('Carlos',5),('Dora',10),('Emilio',15) ]).
timeAvailable(21). 
amountAtTheSameTime(3).

% problem states
initial_state(ctb, ctb(P, [], timeAvailable)) :- people(P).
final_state(ctb([], P, N)) :- N>=0, people(P).

% problem predicates
legal(Current,Limit):-Current=<Limit. % check if the time is enough
