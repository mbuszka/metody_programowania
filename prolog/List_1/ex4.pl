parent/2.
male/1.
female/1.

sibling/2.
sister/2.
grandson/2.
cousin/2.
descendant/2.
is_mother/1.
is_father/1.

male(john).
male(adam).
male(mark).
male(joshua).
female(eve).
female(helen).
female(ivonne).
female(anna).

parent(adam,helen).
parent(adam,ivonne).
parent(adam,anna).
parent(eve,helen).
parent(eve,ivonne).
parent(eve,anna).
parent(john,joshua).
parent(helen,joshua).
parent(ivonne,david).
parent(mark,david).

descendant(X,Y) :- parent(Y,X).
descendant(X,Y) :- parent(Z,X), descendant(Z,Y).

is_father(X) :- parent(X,Y), male(X).
is_mother(X) :- parent(X,Y), female(X).

sibling(X,Y) :- parent(Z,X), parent(Z,Y), X\=Y.

sister(X,Y) :- sibling(X,Y), female(X).

brother(X,Y) :- sibling(X,Y), male(X).

grandson(X,Y) :- parent(Z,X), parent(Y,Z).

cousin(X,Y) :- parent(Z,X), parent(W,Y), sibling(W,Z).
