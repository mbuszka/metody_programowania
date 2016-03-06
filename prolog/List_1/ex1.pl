cat(my_cat).
fish(_) :- fail.
bird(_) :- fail.
worm(_) :- fail.

likes(X,Y) :- cat(X), fish(Y).
likes(X,Y) :- bird(X), worm(Y).
likes(X,Y) :- friend(Y,X).
likes(X,Y) :- friend(X,Y).
friend(my_cat,me).
eats(X,Y) :- X = my_cat, likes(X,Y).


% ans: me
