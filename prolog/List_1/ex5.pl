link\2.
link(wroclaw,warszawa).
link(wroclaw,krakow).
link(wroclaw,szczecin).
link(szczecin,lublin).
link(szczecin,gniezno).
link(warszawa,katowice).
link(gniezno,gliwice).
link(lublin,gliwice).

connection\2.
connection(X,Y) :- link(X,Y).
connection(X,Y) :- link(X,Z), connection(Z,Y).

one_change(X,Y) :- link(X,Z), link(Z,Y).

two_change(X,Y) :- one_change(X,Z), link(Z,Y).
