perm([],[]).
perm(X,[H|T]) :- select(H,X,XS), perm(XS,T).
