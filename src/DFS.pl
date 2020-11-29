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

:- include('Utils.pl').


/* Problem state definition: 

        ctb( <leftSide | rightSide>, PeopleAtLeft, PeopleAtRight, CurrentTime )

    * [Sides         ]: Left or Right, refer to the bridge sides
    * [PeopleAtLeft  ]: People on the left side
    * [PeopleAtRight ]: People on the right side
    * [CurrentTime   ]: Available current time
*/

/* solving the problem using deep first search (DFS). */
solve_dfs( State,_,[] ) :- 
    final_state(State).

solve_dfs( State,History,[Movement|Movements] ) :-
    move(State,Movement),
    update(State,Movement,State2),
    legal(State2),
    not(member(State2,History)),
    write(State),
    solve_dfs(State2,[State2|History],Movements).

test(Problem,Solution) :-
    initial_state(Problem, InitialState),
    solve_dfs(InitialState,[InitialState],Solution).

/* Main functions */
% change between problem states, def: (CurrentState, Limit, Crossers, NewMovement)
move( ctb(leftSide,Left,_,_), Load) :- 
    createGroups(Left,Load).
    % length(Load,M),
    % M =< N,
    % amountAtTheSameTime(N).

move( ctb(rightSide,_,Right,_), _, Load) :- 
    select(X,Right,_),
    Load = [X].

% update the problem state, def: (CurrentState, Crossers, NewState)
update(
    ctb(CurrentSide,PeopleOnTheLeft,PeopleOnTheRight,CurrentTime),
    Load,
    ctb(NewSide,NewLeft,NewRight,NewCurrentTime)
) :-
    update_crossers(CurrentSide, NewSide),                     
    update_sides(Load, CurrentSide, PeopleOnTheLeft, PeopleOnTheRight, NewLeft, NewRight),
    update_time(CurrentTime, NewSide, NewLeft, NewRight, NewCurrentTime).

% update the problem time using the current and new times from the crossers weight
% def: (CurrentTime, bridgeSide, PeopleOnTheLeft, PeopleOnTheRigth, NewCurrentTime)

update_time(CurrentTime, leftSide, PeopleOnTheLeft, _, NewCurrentTime) :-
    sumTimes(PeopleOnTheLeft,RequiredTime),
    NewCurrentTime is CurrentTime - RequiredTime.

update_time(CurrentTime, rightSide, _, PeopleOnTheRight,NewCurrentTime) :-
    sumTimes(PeopleOnTheRight,RequiredTime),
    NewCurrentTime is CurrentTime - RequiredTime.

% change the state where the crossers go trough the bridge
update_crossers(leftSide, rightSide).
update_crossers(rightSide,leftSide).

% update the sides of the bridge
update_sides(Load,leftSide,PeopleOnTheLeft,PeopleOnTheRight,NewLeft,NewRight) :-
    createGroups(Load,PeopleOnTheLeft),        
    subtract(PeopleOnTheLeft, Load, NewLeft),
    insert(Load,PeopleOnTheRight,NewRight). 

update_sides(Load,rightSide,PeopleOnTheLeft,PeopleOnTheRight,NewLeft,NewRight) :-
    selectOne(Load,PeopleOnTheRight,NewRight),       
    insert(Load,PeopleOnTheLeft,NewLeft).  

% problem params
people(alberto, 1).
people(beatriz, 2).
people(carlos,  5).
people(dora,   10).
people(emilio, 15).

timeAvailable( 21 ). 
amountAtTheSameTime( 3 ).

% problem predicates
legal(Current,Limit):-Current=<Limit. % check if the time is enough

% problem start and stop
initial_state(ctb, ctb(leftSide, X, [], Ta)) :- 
    getPeople(X),
    timeAvailable(Ta).

final_state(ctb(rightSide, [], People, N)) :- 
    N >= 0, 
    getPeople(X),
    is_permutation(X,People).