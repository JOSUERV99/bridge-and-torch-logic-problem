/*
Problem:
    Some  friends are walking back to their hostel and
    they  come across a dilapidated old wooden bridge.
    The  bridge  is  very  weak  and  is  only able to
    support the weight of some of them at a time. They
    are  in  a  hurry  since  it is dusk and they must
    cross  in the shortest possible time. They carry a
    flashlight  that allows those who are crossing the
    bridge to see, but it does not serve to illuminate
    the  entire  bridge.  So if several of them cross,
    one   of   the   crossers  must  return  with  the
    flashlight to allow the others to cross.

    Because  they  all  have different fitness levels,
    and  some  are  injured,  it  takes each of them a
    different time to cross the bridge.
*/

% % Si el Estado actual es un estado final, no hay que moverse.
% solve_dfs(Estado,_,[]) :- final_state(Estado).

% /*
%  * Si el Estado actual no es un estado final, genera una movida
%  * para desplazarse a un nuevo estado, y continua la b�squeda a
%  * partir de ese nuevo estado.
%  */
% solve_dfs(Estado,Historia,[Movida|Movidas]) :-
%       move(Estado,Movida),               % generar una nueva Movida
%       update(Estado,Movida,Estado2),     % calcula nuevo estado usando Movida
%       legal(Estado2),                    % nuevo estado debe ser legal
%       not(member(Estado2,Historia)),     % debe ser primera vez que se llega al nuevo estado
%       solve_dfs(Estado2,[Estado2|Historia],Movidas).   % continuar a partir de nuevo estado

:-include('Utils.pl').

% main functions

% change between problem states
move(ctb( leftSide,Left,_,_ ),N,newLeft,Load) :- 
    createGroupsOfN(N,Left,Load),
    subtract(Left,Load,newLeft).

move(ctb( rightSide,_,Right,_ ),newRight,Load) :- 
    select(X,Right,newRight),
    Load = [X].

move(ctb( _,_,_ ),alone).

% problem params
people(alberto, 1).
people(beatriz, 2).
people(carlos,  5).
people(dora,   10).
people(emilio, 15).

timeAvailable( 21 ). 
amountAtTheSameTime( 3 ).

% problem states
initial_state(ctb, ctb(leftSide, X, [], Ta)) :- 
    getPeople(X),
    timeAvailable(Ta).

final_state(ctb(rightSide, [], People, N)) :- 
    N >= 0, 
    getPeople(X),
    is_permutation(People, X).

% problem predicates
legal(Current,Limit):-Current=<Limit. % check if the time is enough


