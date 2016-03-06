even([]).
even([_,_|T]) :- even(T).

palindrom(X) :- reverse(X,X).

singleton([_|[]]).
