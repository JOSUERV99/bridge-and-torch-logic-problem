:- include('CrossingTheBridge.pl').

/* solving the problem using deep first search (DFS). */
solve_dfs( State,_,[] ) :- final_state(State).

solve_dfs( State,History,[Movement|Movements] ) :-
    move(State,Movement),
    update(State,Movement,State2),
    legal(State2),
    not(member(State2,History)),
    solve_dfs(State2,[State2|History],Movements).

test_dfs(Problem,Solution) :-
    initial_state(Problem, InitialState),
    solve_dfs(InitialState,[InitialState],Solution).