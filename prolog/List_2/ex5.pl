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
