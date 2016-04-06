% exercise 1

/* doesn't work for length2([H|T], N). */

length2(L, N) :- var(L), var(N), length2(L, 0, N).
length2(L, N) :- length2(L, 0, N), !.
length2([], A, A).
length2([_|T], A, N) :- A1 is A + 1, length2(T, A1, N).



/* by Piotr Polesiuk */

len(L, N) :- len(L, 0, N).
len([], A, A).
len([_|T], A, N) :- var(N), B is A + 1, len(T, B, N).
len([_|T], A, N) :- integer(N), A < N, B is A + 1, len(T, B, N), !.
	
/* * * * * * * * * * */
	

% exercise 2

connection/2.
connection(wroclaw, warszawa).
connection(warszawa, krakow).
connection(krakow, wroclaw).
connection(gliwice, wroclaw).
connection(wroclaw, katowice).
connection(katowice, warszawa).

trip(S, D, T) :- trip(S, D, [D], T).
trip(S, S, A, A).
trip(S, D, A, T) :- 
	connection(X, D),
	\+member(X, A),
	trip(S, X, [X|A], T).

% exercise 3

bin([0]).
bin([1|X]) :- bin(X,[]).
bin(A, A).
bin(R, T) :-
	bin(R, [H|T]),
	(H = 0; H = 1).

rbin([0]).
rbin(X) :- rbin_(X).
rbin_([1]).
rbin_([H|X]) :- 
	rbin_(X),
	(H = 0; H = 1).

% exercise 4

mirror(leaf, leaf).
mirror(node(L, V, R), node(RP, V, LP)) :- mirror(L,LP), mirror(R, RP).

flatten(T, L) :- flatten(T, [], L).
flatten(leaf, L, L).
flatten(node(L, V, R), L1, S) :- 
	flatten(R, L1, RS),
	flatten(L, [V|RS], S).
	

% exercise 5

insert(E, leaf, node(leaf, E, leaf)).
insert(E, node(L, V, R), node(Lp, V, R)) :- E =< V, !, insert(E, L, Lp).
insert(E, node(L, V, R), node(L, V, Rp)) :- insert(E, R, Rp).

tree([], A, A).
tree([H|T], A, R) :- insert(H, A, S), tree(T, S, R).

treesort(L, R) :- tree(L, leaf, T), flatten(T, R).

% exercise 6

solve(A, C, E, P, R, S, U) :-
    % sublist(X,[A,C,E,P,R,S,U] ).
    permutation([A,C,E,P,R,S,U|_], [0,1,2,3,4,5,6,7,8,9]),
    
    U \= 0, P \= 0,
    concat_number([U,S,A], N1), concat_number([U,S,S,R], N2), concat_number([P,E,A,C,E], N3),
    N3 is N1 + N2, !.

concat_number(L, N) :- concat_number(L, 0, N).
concat_number([], A, A).
concat_number([H|T], A, N) :- A1 is A * 10 + H, concat_number(T, A1, N), !.

% exercise 7

revall(X, X) :- \+is_list(X), !.
revall(L, R) :- revall(L, [], R), !.
revall([], A, A).
revall([H|T], A, R) :- revall(H, HR), revall(T, [HR|A], R), !.
