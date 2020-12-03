:-include('Utils.pl').

solve_best([punto(State,Path,_)|_],_,Moves) :-
    final_state(State), reverse(Path,Moves).

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
    value(S,V),                % calcula valor heur�stico del estado S
    evaluates(States,Values).  % procesa resto de estados
evaluates([],[]).

inserts([Punto|Puntos],Frontier,Frontier1) :-
    insertPoint(Punto,Frontier,Frontier0),  % inserta primer punto
    inserts(Puntos,Frontier0,Frontier1).    % recursivamente inserta los dem�s puntos
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
   initial_state(Problem,State),   % obtener un Estado inicial dado Problema
   value(State,Value),             % calcula el valor heur�stico del estado incial
   solve_best([punto(State,[],Value)],[State],Moves). % inicializa frontera e historial,
                                                      % inicia resoluci�n

%  ////////////////////////////////////////////////////////////////////////////////////////

value(ctb(rightSide,_,PeopleOnTheRight,_),Score) :- 
    bestCrosser(PeopleOnTheRight, RequiredTime),
    length(PeopleOnTheRight, N),
    Score is RequiredTime*RequiredTime - N.

value(ctb(leftSide,PeopleOnTheLeft,_,_),Score)   :- 
    bestCrosser(PeopleOnTheLeft, RequiredTime),
    length(PeopleOnTheLeft, N),
    Score is RequiredTime*RequiredTime - N.

/* Main functions */
% change between problem states, def: (CurrentState, Limit, Crossers, NewMovement)
move(ctb(leftSide,Left,_,_), Load) :- 
    createGroups(Left, Load),
    length(Load, M), amountAtTheSameTime(N),
    M =< N, M > 0.

move(ctb(rightSide,_,Right,_), [X]) :- member(X,Right).

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
final_state(ctb(rightSide, [], People, N)) :- N >= 0, getPeople(X), X=People.

% problem predicates
legal( ctb(_, _, _, CurrentTime) ) :- CurrentTime >= 0.
