/* 
    Function and utils...
        by: JosueRV99
*/

sublist([], L).
sublist([X|Xs], [X|Ys]) :- sublist(Xs, Ys).
sublist(Xs, [_|Ys]) :- sublist(Xs, Ys).

% generate all the groups not repeated with N elements
createGroupsOfN(N, List, Result) :- 
    findall(X, (sublist(X,List),length(X,N)), R),
    setof(Y, member(Y,R), Result).

% select one element and the rest its returned too...
selectOne(X,[X|Xs],Xs).                          
selectOne(X,[Y|Ys],[Y|Zs]) :- selectOne(X,Ys,Zs).     

% sum all the times in a group
sumTime([],S,S).
sumTime([(N,T)|Group], Acc, S) :- Sum is Acc + T, sumTime(Group,Sum,S).