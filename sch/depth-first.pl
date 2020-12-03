/*
 * solve_dfs(State,History,Moves)
 *   Moves es la secuencia de movidas requeridas para
 *   alcanzar un estado final deseado a partir de State.
 *   History contiene los estados previamente visitados.
 */
 
% Si el Estado actual es un estado final, no hay que moverse.
solve_dfs(Estado,_,[]) :- final_state(Estado).

/*
 * Si el Estado actual no es un estado final, genera una movida
 * para desplazarse a un nuevo estado, y continua la b�squeda a
 * partir de ese nuevo estado.
 */
solve_dfs(Estado,Historia,[Movida|Movidas]) :-
      move(Estado,Movida),               % generar una nueva Movida
      update(Estado,Movida,Estado2),     % calcula nuevo estado usando Movida
      legal(Estado2),                    % nuevo estado debe ser legal
      not(member(Estado2,Historia)),     % debe ser primera vez que se llega al nuevo estado
      solve_dfs(Estado2,[Estado2|Historia],Movidas).   % continuar a partir de nuevo estado

/*
 * Inicializa un problema y lo resuelve.
 *   Problema: nombre del problema.
 *   Movidas: movidas requeridas para resolver el problema.
 */
test_dfs(Problema,Movidas) :-
      initial_state(Problema,Estado),      % obtener un Estado inicial dado Problema
      solve_dfs(Estado,[Estado],Movidas).  % inicia resoluci�n desde Estado

/*
 * El problema del ma�z, la fgallina y la zorra se identifica con el �tomo zgm.
 * El estado tiene la siguiente estructura:
 *    zgm(PosicionBote, CosasEnRiveraIzquierda, CosasEnRiveraDerecha)
 * Las listas de ambas riveras deben estar ordenadas en la secuencia
 *    zorra, gallina, maiz
 * para evitar repetici�n de estados.
 */

% En el estado incial el bote est� a la izquierda, todas las cosas est�n en la
% rivera izquierda y no hay nada a la derecha.
initial_state(zgm,zgm(izq,[zorra,gallina,maiz],[])).

% En el estado final el bote est� a la derecha, todas las cosas est�n en la
% rivera derecha y no hay nada a la izquierda.
final_state(zgm(der,[],[zorra,gallina,maiz])).

/* Move genera movidas a partir de un estado
 * Bote a la izquierda, tomar cualquier cosa que est� a la izquierda
 * Bote a la derecha, tomar cualquier cosa que est� a la derecha
 * Bote en cualquier rivera, el granjero se puede devolver solo
 */
move(zgm(izq,I,_),Carga):-member(Carga,I).
move(zgm(der,_,D),Carga):-member(Carga,D).
move(zgm(_,_,_),solo).


% Update actualiza un estado dada una movida.
update(zgm(B,I,D),Carga,zgm(B1,I1,D1)):-
      update_Bote(B,B1),                     % cambiar la rivera del bote
      update_margenes(Carga,B,I,D,I1,D1).    % modificar riveras de acuerdo
                                             % con lo transportado

% Actualizar el bote es cambiarlo de una rivera a otra.
update_Bote(izq,der).
update_Bote(der,izq).


/*
 * Actualizar las m�rgenes consiste en trasladar lo que fue transportado
 * en el bote quit�ndolo de la rivera d�nde se tom� y poni�ndolo en la otra.
 *
 * update_margenes(Movida, RiveraIzquierdaVieja, RiveraDerechaVieja,
 *                         RiveraIzquierdaNueva, RiveraDerechaNueva)
 */
% Si no se traslad� nada,
%     no importa d�nde estaba el bote, las riveras quedan igual
update_margenes(solo,_,I,D,I,D). % No hay cambio porque no se traslad� nada.

% Bote arranc� de la izquierda, trasladar cosa de izquierda a derecha.
update_margenes(Carga,izq,I,D,I1,D1):-
      select(Carga,I,I1),        % Quita de la rivera izquierda lo trasladado
      insert(Carga,D,D1).        % Inserta lo trasladado en la rivera derecha

% Bote arranc� de la izquierda, trasladar cosa de izquierda a derecha.
update_margenes(Carga,der,I,D,I1,D1):-
      select(Carga,D,D1),        % Quita de la rivera derecha lo trasladado
      insert(Carga,I,I1).        % Inserta lo trasladado en la rivera izquierda


/*
 * insert(ElementoInsertado, ListaVieja, ListaNueva)
 *
 * Inserta en orden una de las cosas en una lista.
 * La relaci�n precedes/2 establece el orden de las cosas: z < g < m.
 */
insert(X,[Y|Ys],[X,Y|Ys]):-precedes(X,Y).   % Elemento va al inicio
insert(X,[Y|Ys],[Y|Zs]):-precedes(Y,X),insert(X,Ys,Zs).  % Insertar m�s adentro.
insert(X,[],[X]).                           % Insertar como �nico elemento.


/*
 * select(Elemento, ListaQueContieneElemento, ListaSinElemento)
 *
 * Extrae no determisticamente un elemento de una lista que lo contiene
 * y obtiene la lista sin ese elemento.
 */
select(X,[X|Xs],Xs).                          % Extrae primer elemento.
select(X,[Y|Ys],[Y|Zs]):-select(X,Ys,Zs).     % Extrae elemento de m�s adentro.


/*
 * Establece el ordenamiento z < g < m, requerido para mantener las
 * riveras ordenadas de modo que no se consideren distintos dos estados
 * simplemente porque son permutaciones del mismo conjunto.
 */

/* Caso no determin�stico.
   Las cl�usulas no son excluyentes:
       ?-precedes(zorra,maiz).
   genera dos veces la misma soluci�n.
*/
% precedes(zorra,_).
% precedes(_,maiz).

/* Caso determin�stico.
   Las cl�usulas son excluyentes,
   nunca generan dos veces la misma soluci�n.
*/

precedes(zorra,gallina).
precedes(zorra,maiz).
precedes(gallina,maiz).


% Se revisa la legalidad de la rivera en la que no est� el granjero.
legal(zgm(izq,_,D)):-not(ilegal(D)). % granjero a la izq., revisar rivera der.
legal(zgm(der,I,_)):-not(ilegal(I)). % granjero a la der., revisar rivera izq.

% ilegal(Lista) indica si hay problemas
ilegal(L):-member(zorra,L),member(gallina,L). %% z y g no pueden estar solos
ilegal(L):-member(gallina,L),member(maiz,L).  %% g y m no pueden estar solos

