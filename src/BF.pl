:-include('CrossingTheBridge.pl').

/* solving the problem using best search approach (BF). */
solve_best([punto(State,Path,_)|_],_,Moves) :-
    final_state(State), reverse(Path,Moves).

solve_best([punto(State,Path,_)|Frontier],History,FinalPath) :-
    findall(M,move(State,M),Moves),                  % generates all the movements of the best state
    updates(Moves,Path,State,States),                % get the new states using the movements
    legals(States,States1),                          % choose the new states that are legals
    news(States1,History,States2),                   % delete new states already included in the history
    evaluates(States2,Values),                       % calculate heuristic values of the new states
    inserts(Values,Frontier,Frontier1),              % insert in order the new points in the frontier
    solve_best(Frontier1,[State|History],FinalPath). % continue from new frontier

updates([M|Ms],Path,S,[(S1,[M|Path])|Ss]) :-
    update(S,M,S1),         % obtains the state reached by a move
    updates(Ms,Path,S,Ss).  % recursively continue with the another states
updates([],_,_,[]).

% check if the given states are legal
legals([(S,P)|States],[(S,P)|States1]) :-
    legal(S),
    legals(States,States1).
legals([(S,_)|States],States1) :-
    not(legal(S)),
    legals(States,States1).
legals([],[]).

% filter the repeated cases
news([(S,_)|States],History,States1) :-
    member(S,History),
    news(States,History,States1).
news([(S,P)|States],History,[(S,P)|States1]) :-
    not(member(S,History)),
    news(States,History,States1).
news([],_,[]).

% evaluates every pair, using value as score
evaluates([(S,P)|States],[punto(S,P,V)|Values]) :-
    value(S,V),                
    evaluates(States,Values). 
evaluates([],[]).

% insert in order every pair
inserts([Punto|Puntos],Frontier,Frontier1) :-
    insertPoint(Punto,Frontier,Frontier0), 
    inserts(Puntos,Frontier0,Frontier1).   
inserts([],Frontier,Frontier).

% insert a point into a point list, using the less_than and equals clauses
insertPoint(Point,[],[Point]).
insertPoint(Point,[Point1|Points],[Point1,Point|Points]) :- less_than(Point1,Point).
insertPoint(Point,[Point1|Points],[Point|Points]) :- equals(Point,Point1).
insertPoint(Point,[Point1|Points],[Point1|Points1]) :-
    less_than(Point,Point1),
    insertPoint(Point,Points,Points1).
insertPoint(Point,[Point1|Points],[Point,Point1|Points]) :- same(Point,Point1).

% point order oprators
equals(punto(S,_,V),punto(S,_,V)).
less_than(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 < V2.
same(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 = V2.

% testing...
test_best_search(Problem,Moves) :-
   initial_state(Problem,State),  
   value(State,Value),            
   solve_best([punto(State,[],Value)],[State],Moves). 