%ex4

even([]).
even([_,_|T]) :- even(T).

palindrom(X) :- reverse(X,X).

singleton([_|[]]).

%reverse([],[]).
%reverse([H,T],[S|[H]) :- reverse(T,S). 

% ex5

head(H,[H|_]).

last([L|[]],L).
last([_|T],L) :- last(T,L).

tail(T,[_|T]).

init([_],[]).
init([H|TLS],[H|TIS]) :- init(TLS,TIS).

prefix([],_).
prefix([H|PS],[H|LS]) :- prefix(PS,LS).

suffix(S,S).
suffix([_|T],S) :- suffix(T,S).

% ex6

sublist([],[]).
sublist([_|L],S) :-sublist(L,S).
sublist([H|L],[H|S]) :- sublist(L,S).

% ex7

perm([],[]).
perm(X,[H|T]) :- select(H,X,XS), perm(XS,T).
