/* Function and utils, by: JosueRV99 */

% generate all the sublists
sublist([], []).
sublist([X|Xs], [X|Ys]) :- sublist(Xs, Ys).
sublist(Xs, [_|Ys]) :- sublist(Xs, Ys).

% generate all the groups not repeated with N elements
createGroups(List, Group) :- 
    findall(X, sublist(X,List), R), setof(Y, member(Y,R), Result),member(Group, Result).

% select one element and the rest its returned too...
selectOne(X,[X|Xs],Xs).                          
selectOne(X,[Y|Ys],[Y|Zs]) :- selectOne(X,Ys,Zs).     

% find the max time from a [name, time] list using time
maxTime([],0).
maxTime([[_,T1]|Tail],Max) :-
    maxTime(Tail,TailMax),
    T1 > TailMax, Max is T1.
maxTime([[_,T1]|Tail],Max) :-
    maxTime(Tail,TailMax),
    T1 =< TailMax, Max is TailMax.

is_permutation(Xs, Ys) :-
    msort(Xs, Sorted),
    msort(Ys, Sorted).

getPeople(X) :-
    findall( [P,T], people(P,T), X).