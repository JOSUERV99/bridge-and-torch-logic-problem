:- include('CrossingTheBridge.pl').

/* solving the problem using hill climb approach (HC). */
solve_hill_climb( State,_,[] ) :- final_state(State).

solve_hill_climb(State,History,[Move|Moves]) :-
    hill_climb(State,Move),
    move(State, Move),
    update(State,Move,State1),
    legal(State1),
    not(member(State1,History)),
    solve_hill_climb(State1,[State1|History],Moves).

/* Main functions */
hill_climb(State,Move) :-
    findall(M,move(State,M),Moves),         
    evaluate_and_order(Moves,State,[],MVs), 
    member((Move,_),MVs).                  

evaluate_and_order([Move|Moves],State,MVs,OrderedMVs) :-
    update(State,Move,State1),         
    value(State1,Value),               
    insertPair((Move,Value),MVs,MVs1), 
    evaluate_and_order(Moves,State,MVs1,OrderedMVs).  
    
evaluate_and_order([],_,MVs,MVs).

insertPair(MV,[],[MV]).
insertPair((M,V),[(M1,V1)|MVs],[(M,V),(M1,V1)|MVs]) :- V < V1.
insertPair((M,V),[(M1,V1)|MVs],[(M1,V1)|MVs1]) :- V >= V1, insertPair((M,V),MVs,MVs1).

test_hill_climb(Problem,Moves) :-
   initial_state(Problem,State),
   solve_hill_climb(State,[State],Moves).