sublist([],[]).
sublist([_|L],S) :-sublist(L,S).
sublist([H|L],[H|S]) :- sublist(L,S).
