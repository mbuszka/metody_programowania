meets(X,Y) :- animal(X), human(Y) , lives_in_zoo(X), visits_zoo(Y).
meets(X,Y) :- meets(Y,X).
nice(X) :- human(X), visits_zoo(X).
happy(Y) :- animal(Y), nice(X), human(X), meets(X,Y).
not_happy(X) :- lives_in_zoo(X), dragon(X).


happy(X :- dragon(X).
not_animal(X) :- dragon(X).
