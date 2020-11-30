/* 
    Function and utils...
        by: JosueRV99
*/

% generate all the sublists
sublist([], []).
sublist([X|Xs], [X|Ys]) :- sublist(Xs, Ys).
sublist(Xs, [_|Ys]) :- sublist(Xs, Ys).

% generate all the groups not repeated with N elements
createGroups(List, Z) :- 
    findall(X, sublist(X,List), R),
    setof(Y, member(Y,R), Result),
    member(Z, Result).

% select one element and the rest its returned too...
selectOne(X,[X|Xs],Xs).                          
selectOne(X,[Y|Ys],[Y|Zs]) :- selectOne(X,Ys,Zs).     

% find the max time from a [name, time] list using time
maxTime([],0).
maxTime([[_,T1]|Tail],Max) :-
    maxTime(Tail,TailMax),
    T1 > TailMax,
    Max is T1.
maxTime([[_,T1]|Tail],Max) :-
    maxTime(Tail,TailMax),
    T1 =< TailMax,
    Max is TailMax.

is_permutation(Xs, Ys) :-
    msort(Xs, Sorted),
    msort(Ys, Sorted).

show_solution([]).
show_solution([ ctb(rightSide,PeopleOnTheLeft,PeopleOnTheRight,CurrentTime) | Ss ]) :- 
    write('\n['),
    show_people(PeopleOnTheLeft), 
    write(']\t['), 
    show_people(PeopleOnTheRight), 
    write(']\t\n(Current time:'), write(CurrentTime), write(')\n'),
    show_solution(Ss).

show_people([]).
show_people([[N,_]|People]) :- write(N), write(' '), show_people(People).

getPeople(X) :-
    findall( [P,T], people(P,T), X).