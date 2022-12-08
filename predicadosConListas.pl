% 1.- Pertenece: verificar si un elemento pertenece a una lista dada.
% Si el numero a buscar está en la prinera posicion de la lista entonces se cumple la condicion
pertenece(X, [X|_]).
%si e primer elemento de la lista no es igual al numero a buscar entonces llamamo de nuevo a la funcion
pertenece(X,[_|Y]):- pertenece(X,Y).

% 2.- size: obener la longitud de una lista
%si solo se tiene un elemento el valor del sizxe es 1 
size([_],1).
%si no se tiene ningun elemento el valor del size es 0
size([],0).
% si se tienen mas elementos entonces tomamos la cola de la cadena y con una varibale auxiliar
%vamos a guardar el valor del size para que cada paso por la funcion este aumente en 1.
size([_|Xs],R):- size(Xs,Aux), R is Aux+1.

% 3.- concatenar: obtener la concatenacion de 2 listas dadas
%si a una lista la concatenamos con una lista vacia nos regresa la misma lista.
concatenar(L1,[],L1).
% si a una lista vacia la concatenamos con una lista nos regresara la lista.
concatenar([],L2,L2).
% si no se cumplen estas condiciones tomamos el primer valor de la lista y lo concatenamos con el resultado
%y este entra en recursividad para cuando la priemra lista este vacia se una toda la segunda cadena 
%y regrese toda la cadena ya concatenada
concatenar([H1|T1], L2, [H1|R]):- concatenar(T1,L2,R).

% 4.- Eliminar: Eliminar un elemento de una lista
%si la lista esta vacia no se elimina nada
eliminar(_,[],[]).
%si el elemento a eliminar es el mismo que el primer elemento de la lista, entonces regresamos el resto de la 
%lista
eliminar(E, [E|Tail],Tail).
%separamos la lista en cabeza y cola y con ayuda de la recursividad pasamos la cola a la misma funcion
%al final retornamos la lista ya sin ese elemnento
eliminar(E, [X|Tail], [X|R]):- eliminar(E, Tail,R).

% 5.- agregar un elemento a una lista
% tenemos el elemento y la lista, al final retonarmos el elemento concatenado con la lista
agregar(X,L,[X|L]).

% 6.- par e impares: separar una lista en 2 listas (pares e impares).
%si la lista esta vacia se retorna una losta vacia
separar([],[],[]).
%si el primer elemento de la lista es par entonces lo argegamos a la lista de pares y volvemos a llamar
%a la funcion pero ahora con la cola de la lista
separar([X|Xs],[P|Ps],Is) :- 
    X mod 2 =:=0,
    P is X,
    separar(Xs,Ps,Is).
% si el elemnento no es par, osea que es impar entonces se agrega a la lista de imapres
%despues vovlemos a llamar a la funcion con la cola de la lista
separar([X|Xs],Ps,[I|Is]) :-
    X mod 2 =\=0,
    I is X,
    separar(Xs,Ps,Is).

% 7 .- Ascendente: Verificar si una lista se encuentra de manera ascendente.
%si la lista solo tiene un elemento entonces esta bien
asc([_]).
% tomamos los 2 primeros elementos de la lista y los comapramos, si el 2do es mayor al primero
%entonces pasamos el segundo elemento y la cola a la funcion, cuando uno no sea mayor a la cabeza
%significa que no esta ordenada de oforma ascendente
asc([X1,X2|Tail]):- X1<X2, asc([X2|Tail]).

% 8 .- Descendente: Verificar si una lista se encuentra de manera descendente.
%si solo hay un elemento no hay problema.
des([_]).
% tomamos los 2 primeros elementos de la lista y los comapramos, si el 2do es menor al primero
%entonces pasamos el segundo elemento y la cola a la funcion, cuando uno no sea menor a la cabeza
%significa que no esta ordenada de oforma descendente
des([X1,X2|Tail]):- X1>X2, des([X2|Tail]).

% 9 .- Aplanar: aplanar(L, A), donde L es en general una lista de listas,
% tan compleja en su anidamiento como queramos imaginar, y A es la lista que resulta
% de reorganizar los elementos contenidos en las listas anidadas en un único nivel.
%si la lista esta vacia retorna una lista vacia
aplanar([],[]).
% separamos la lista y comrpobamos si el primer elemento es atomico(osea que no es una lista) y si es asi
%concatenamos el elemento con el resultado y llamamos a la misma funcion con la cola
aplanar([X|R], [X|P]) :- atomic(X), aplanar(R,P).
%si no es atomico el elemento entonces aplanamos la lista nueva y al final la agregamos al resultado
aplanar([X|R],P) :- not(atomic(X)), aplanar(X, P_X), aplanar(R, P_R), append(P_X, P_R, P).

% 10 .- Menor que Head: obtener uns lista con elementos menores que el elemento Head.
%si la lista esta vacia regresa una ista vacia
menorHead([],[]).
%si solo hay un elemento retornara la lista vacia
menorHead([_],[]).
%separamos los 2 primeros elementos de la lista y la cola, comprobamos si el segundo elemento 
%es menor al primero, si es asi concatenamos el segundo elemento a la lista final y llamamos a la
%funcion con el primer elemento y la cola de la lista
menorHead([X1,X2|Tail],[X2|L]) :- X1>X2, menorHead([X1|Tail],L).
%si el segundo elemento es mayor entonces solo llamamos a la funcion con el priemr elemento y la cola
menorHead([X1,X2|Tail],L) :- X1<X2, menorHead([X1|Tail],L).
%si son iguales entonces ahcemos lo mismo que la regla anterior
menorHead([X1,X2|Tail],L) :- X1=:=X2, menorHead([X1|Tail],L).

% 11 .- Mayor que Head: obtener uns lista con elementos mayores que el elemento Head.
%si la lista esta vacia regresa una ista vacia
mayorHead([],[]).
%si solo hay un elemento retornara la lista vacia
mayorHead([_],[]).
%separamos los 2 primeros elementos de la lista y la cola, comprobamos si el segundo elemento 
%es mayor al primero, si es asi concatenamos el segundo elemento a la lista final y llamamos a la
%funcion con el primer elemento y la cola de la lista
mayorHead([X1,X2|Tail],[X2|L]) :- X1<X2, mayorHead([X1|Tail],L).
%si el segundo elemento es menor entonces solo llamamos a la funcion con el priemr elemento y la cola
mayorHead([X1,X2|Tail],L) :- X1>X2, mayorHead([X1|Tail],L).
%si son iguales entonces ahcemos lo mismo que la regla anterior
mayorHead([X1,X2|Tail],L) :- X1=:=X2, mayorHead([X1|Tail],L).

% 12 .- Invertir: obtener una lista con los elementos invertidos.
% si la lista esta vacia entonces regresamos una lista vacia
invierte([],[]).
%separamos la cabeza y la cola de la lista y llamamos a la misma funcion junto y retorna la variable R
%despues de eso llamamos a la funcion para concatenar que le pasamos R el primer elemento dentro de una lista
%de un elemento y nos retorna l
invierte([H|T],L):- invierte(T,R), concat(R,[H],L).

% si el primer elemento esta vacio nos regresa la 2da lista
concat([],L,L).
%separa la pimera lista en el primer elemento y la cola y el primer elemento se concatena con la lista
%resultante y se vuelve a llamar a la funcion con la cola de la primer lista
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).
