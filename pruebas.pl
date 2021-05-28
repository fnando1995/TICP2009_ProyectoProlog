%reglas%
conexion(d,f,4).
conexion(h,f,9).
conexion(i,f,11).
conexion(f,g,10).
conexion(f,a,8).
conexion(a,b,7).
conexion(d,i,2).
%"funciones"%
%resuelve S/N el nodo X tiene aristas o conexiones con otros nodos%
tieneArista(X) :-conexion(X,_,_).
%resuelve el costo del camino entre inicio y destino pasando por intermedio%
llegar(inicio,destino,intermedio,costo):-
    conexion(inicio,int,C1),conexion(int,destino,C2),costo is C1+C2.
%resuelve S/N de manera recursiva si un nodo tiene camino a otro%
camino(X,Y):- conexion(X,Y,_).
camino(X,Y):- conexion(X,Z,_) , camino(Z,Y).