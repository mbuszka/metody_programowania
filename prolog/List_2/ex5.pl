%head(H,[H|_]).

%last(L,H) :- reverse(L,[H|_]).

%tail(T,[_|T]).

%init(L,T) :- reverse(T,X), reverse(L,Lr), tail(X,Lr).

%prefix([],_).
%prefix([H|PS],[H|LS]) :- prefix(PS,LS).

%suffix(L,S) :- reverse(L,Lr), reverse(S,Sr), prefix(Sr,Lr).
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
