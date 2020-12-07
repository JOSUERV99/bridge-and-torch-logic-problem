:- include('CrossingTheBridge.pl').

/* solving the problem using hill climb approach (HC). */
solve_hill_climb( State,_,[] ) :- final_state(State).

solve_hill_climb(State,History,[Move|Moves]) :-
    hill_climb(State,Move),                         % generate all the movements, sort using value and return every movement according to the given score
    update(State,Move,State1),                      % update the state using the movement
    legal(State1),                                  % check if the reached stated is legal
    not(member(State1,History)),                    % not repeating states
    solve_hill_climb(State1,[State1|History],Moves).

% Hill climb approach: generate all the movements and sorting from better to worst movement 
hill_climb(State,Move) :-
    findall(M,move(State,M),Moves),         % generate all the movements from the state
    evaluate_and_order(Moves,State,[],MVs), % evaluate and order every movement using a score
    member((Move,_),MVs).                   % give every movement fron the pairs

% evaluate and order every generated movement
evaluate_and_order([Move|Moves],State,MVs,OrderedMVs) :-
    update(State,Move,State1),                      % update the state using the movement
    value(State1,Value),                            % evaluate the new state
    insertPair((Move,Value),MVs,MVs1),              % insert a pair in order
    evaluate_and_order(Moves,State,MVs1,OrderedMVs).
evaluate_and_order([],_,MVs,MVs).

% insert a pair (Movement, Value)
insertPair(MV,[],[MV]).
insertPair((M,V),[(M1,V1)|MVs],[(M,V),(M1,V1)|MVs]) :- V < V1.
insertPair((M,V),[(M1,V1)|MVs],[(M1,V1)|MVs1]) :- V >= V1, insertPair((M,V),MVs,MVs1).

% testing...
test_hill_climb(Problem,Moves) :-
   initial_state(Problem,State),
   solve_hill_climb(State,[State],Moves).