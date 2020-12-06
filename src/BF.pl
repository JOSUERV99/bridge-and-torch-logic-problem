:-include('CrossingTheBridge.pl').

solve_best([punto(State,Path,_)|_],_,Moves) :-
    final_state(State),reverse(Path,Moves).

solve_best([punto(State,Path,_)|Frontier],History,FinalPath) :-
    findall(M,move(State,M),Moves),                  % obtiene movidas del mejor estado
    updates(Moves,Path,State,States),                % obtiene los nuevos estados usando movidas
    legals(States,States1),                          % escoge los nuevos estados que son legales
    news(States1,History,States2),                   % elimina nuevos estados ya incluidos en historial
    evaluates(States2,Values),                       % calcula valores heur�sticos de los nuevos estados
    inserts(Values,Frontier,Frontier1),              % inserta en orden los nuevos puntos en la frontera
    solve_best(Frontier1,[State|History],FinalPath). % continuar a partir de nueva frontera

updates([M|Ms],Path,S,[(S1,[M|Path])|Ss]) :-
    update(S,M,S1),         % obtiene el estado al que se llega por una movida
    updates(Ms,Path,S,Ss).  % procesa recursivamente las siguientes movidas
updates([],_,_,[]).

legals([(S,P)|States],[(S,P)|States1]) :-
    legal(S),
    legals(States,States1).
legals([(S,_)|States],States1) :-
    not(legal(S)),
    legals(States,States1).
legals([],[]).

news([(S,_)|States],History,States1) :-
    member(S,History),
    news(States,History,States1).
news([(S,P)|States],History,[(S,P)|States1]) :-
    not(member(S,History)),
    news(States,History,States1).
news([],_,[]).

evaluates([(S,P)|States],[punto(S,P,V)|Values]) :-
    value(S,V),                
    evaluates(States,Values). 
evaluates([],[]).

inserts([Punto|Puntos],Frontier,Frontier1) :-
    insertPoint(Punto,Frontier,Frontier0), 
    inserts(Puntos,Frontier0,Frontier1).   
inserts([],Frontier,Frontier).
insertPoint(Point,[],[Point]).

insertPoint(Point,[Point1|Points],[Point1,Point|Points]) :- less_than(Point1,Point).
insertPoint(Point,[Point1|Points],[Point|Points]) :- equals(Point,Point1).

insertPoint(Point,[Point1|Points],[Point1|Points1]) :-
    less_than(Point,Point1),
    insertPoint(Point,Points,Points1).

insertPoint(Point,[Point1|Points],[Point,Point1|Points]) :- same(Point,Point1).

equals(punto(S,_,V),punto(S,_,V)).
less_than(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 < V2.
same(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 = V2.

test_best_search(Problem,Moves) :-
   initial_state(Problem,State),  
   value(State,Value),            
   solve_best([punto(State,[],Value)],[State],Moves). 

test(S) :-
    test_best_search(ctb,S).