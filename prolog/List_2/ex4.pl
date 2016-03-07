even([]).
even([_,_|T]) :- even(T).

palindrom(X) :- reverse(X,X).

singleton([_|[]]).

%reverse([],[]).
%reverse([H,T],[S|[H]) :- reverse(T,S). 
