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

% sum all the times in a group
sumTime([],S,S).
sumTime([[_,T]|Group], Acc, S) :- 
    Sum is Acc + T, 
    sumTime(Group,Sum,S).

% insert elements in a list
insert([Nn,Nt], [[Yn,Yt]|Ys], [[Nn,Nt],[Yn,Yt]|Ys]) :- 
    precedes([Nn,Nt],[Yn,Yt]).   
insert([Nn,Nt], [[Yn,Yt]|Ys], [_|Zs]) :- 
    precedes([Yn,Yt],[Nn,Nt]),
    insert([Nn,Nt],Ys,Zs).  
insert([Nn,Nt], [], [[Nn,Nt]]).                         

% stablish a logic sequence
precedes([N1,T1],[N2,T2]) :-
    N1 \= N2,
    T1 > T2.

is_permutation(Xs, Ys) :-
    msort(Xs, Sorted),
    msort(Ys, Sorted).

getPeople(X) :-
    findall( [P,T], people(P,T), X).