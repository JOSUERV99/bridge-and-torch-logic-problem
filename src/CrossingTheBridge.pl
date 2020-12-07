
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

Problem state definition: 

        ctb( <leftSide | rightSide>, PeopleAtLeft, PeopleAtRight, CurrentTime )

    * [Sides         ]: Left or Right, refer to the bridge sides
    * [PeopleOnTheLeft  ]: People on the left side
    * [PeopleOnTheRight ]: People on the right side
    * [CurrentTime   ]: Available current time
*/

:- include('Utils.pl').

% problem params

% case 1
% people(alberto, 1).
% people(beatriz, 2).
% people(carlos,  5).
% people(dora,   10).
% people(emilio, 15).
% timeAvailable( 28 ). 
% amountAtTheSameTime( 2 ).

% case 2
% people(alberto, 1).
% people(beatriz, 2).
% people(carlos,  5).
% people(dora,   10).
% people(emilio, 15).
% timeAvailable( 21  ). 
% amountAtTheSameTime( 3 ).

% case 3
% people(alberto, 1).
% people(beatriz, 2).
% people(carlos,  5).
% people(dora,   10).
% people(emilio, 15).
% people(julio,  20).
% timeAvailable( 42  ). 
% amountAtTheSameTime( 2 ).

% % case 4
people(alberto, 1).
people(beatriz, 2).
people(carlos,  5).
people(dora,   10).
people(emilio, 15).
people(julio,  20).
timeAvailable( 30 ). 
amountAtTheSameTime( 3 ).

%value(ctb(rightSide, [], People, N), Score) :- getPeople(People), N >= 0, Score is -N*N.
value(ctb(leftSide, [], People, N), Score) :- getPeople(People), N >= 0, Score is 0.

value(ctb(rightSide,_,PeopleOnTheRight,_),Score) :- 
    bestCrosser(PeopleOnTheRight, BestTime),
    length(PeopleOnTheRight, N),
    Score is BestTime - N*N.

value(ctb(leftSide,PeopleOnTheLeft,_,_),Score)   :- 
    bestCrosser(PeopleOnTheLeft, BestTime),
    length(PeopleOnTheLeft, N),
    Score is BestTime - N*N.

% change between problem states, def: (CurrentState, Limit, Crossers, NewMovement)
move(ctb(leftSide,PeopleOnTheLeft,_,_), Load) :- 
    createGroups(PeopleOnTheLeft, Load),
    length(Load, M), 
    amountAtTheSameTime(N),
    M =< N, M > 0.

move(ctb(rightSide,[],PeopleOnTheRight,_), []) :- getPeople(PeopleOnTheRight).
move(ctb(rightSide,_,PeopleOnTheRight,N), [X]) :- member(X,PeopleOnTheRight), N > 0.

% update the problem state, def: (CurrentState, Crossers, NewState)
update(
    ctb(CurrentSide,PeopleOnTheLeft,PeopleOnTheRight,CurrentTime),
    Load,
    ctb(NewSide,NewLeft,NewRight,NewCurrentTime)
) :-
    update_crossers(CurrentSide, NewSide),                  
    update_sides(Load, CurrentSide, PeopleOnTheLeft, PeopleOnTheRight, NewLeft, NewRight),
    update_time(CurrentTime, Load, NewCurrentTime).

% update the problem time using the current and new times from the crossers weight
update_time(CurrentTime, Load, NewCurrentTime) :-
    maxTime(Load, MaxTime),
    NewCurrentTime is CurrentTime - MaxTime.

% change the state where the crossers go trough the bridge
update_crossers(leftSide, rightSide).
update_crossers(rightSide,leftSide).

% update the sides of the bridge
update_sides(Load,leftSide,PeopleOnTheLeft,PeopleOnTheRight,NewLeft,NewRight) :-      
    subtract(PeopleOnTheLeft, Load, NewLeft),
    insertAll(Load, PeopleOnTheRight, NewRight).

update_sides([X],rightSide,PeopleOnTheLeft,PeopleOnTheRight,NewLeft,NewRight) :-
    subtract(PeopleOnTheRight, [X], NewRight),    
    insert(X, PeopleOnTheLeft, NewLeft).

% problem start and stop
initial_state(ctb, ctb(leftSide, X, [], Ta)) :- getPeople(X), timeAvailable(Ta).
final_state(ctb(rightSide, [], People, N)) :- N >= 0, getPeople(People).

% problem predicates
legal( ctb(_, _, _, CurrentTime) ) :- CurrentTime >= 0.