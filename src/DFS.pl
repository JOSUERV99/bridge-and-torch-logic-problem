:- include('Utils.pl').

/* solving the problem using deep first search (DFS). */
solve_dfs( State,History,[] ) :- 
    final_state(State),
    reverse(History, Path),
    write(Path).

solve_dfs( State,History,[Movement|Movements] ) :-
    move(State,Movement),
    update(State,Movement,State2),
    legal(State2),
    not(member(State2,History)),
    solve_dfs(State2,[State2|History],Movements).

test_dfs(Problem,Solution) :-
    initial_state(Problem, InitialState),
    solve_dfs(InitialState,[InitialState],Solution).

/* Main functions */
% change between problem states, def: (CurrentState, Limit, Crossers, NewMovement)
move(ctb(leftSide,Left,_,_), Load) :- 
    createGroups(Left, Load),
    length(Load, M), amountAtTheSameTime(N),
    M =< N, M > 0.

move(ctb(rightSide,_,Right,_), Load) :- 
    select(X,Right,_), Load = [X].

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
% def: (CurrentTime, bridgeSide, PeopleOnTheLeft, PeopleOnTheRigth, NewCurrentTime)
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

update_sides(Load,rightSide,PeopleOnTheLeft,PeopleOnTheRight,NewLeft,NewRight) :-
    subtract(PeopleOnTheRight, Load, NewRight),    
    insertAll(Load, PeopleOnTheLeft,  NewLeft).

% problem params
people(alberto, 1).
people(beatriz, 2).
people(carlos,  5).
people(dora,   10).
people(emilio, 15).

timeAvailable( 21 ). 
amountAtTheSameTime( 3 ).

% problem start and stop
initial_state(ctb, ctb(leftSide, X, [], Ta)) :- getPeople(X), timeAvailable(Ta).
final_state(ctb(rightSide, [], People, N)) :- N >= 0, getPeople(X), is_permutation(X,People).

% problem predicates
legal( ctb(_, _, _, CurrentTime) ) :- CurrentTime >= 0.