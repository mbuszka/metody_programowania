% exercise 1

perm([],[]).
perm([H|T],X) :- perm(T,P), select(H,X,P).

% exercise 2

filter([], []).
filter([H|T], [H|S]) :- H >= 0, !, filter(T, S).
filter([_|T], S) :- filter(T, S).


count(_, [], 0).
count(H, [H|T], N) :- count(H, T, M), N is M + 1, !.
count(E, [_|T], N) :- count(E, T, N).

exp1(B, E, R) :- R is B ^ E.

% exp2(_, 0, 1).
% exp2(B, 1, B).
exp2(B, E, R) :- e(B, E, 1, R).
e(_, 0, A, A).
e(B, E, A, R) :- E2 is E - 1, A2 is A * B, e(B, E2, A2, R), !.

% exercise 3

factorial(N, R) :- f(N, R, 1).
f(0, A, A).
f(N, R, A) :- N1 is N - 1, A1 is A * N, f(N1, R, A1), !.

concat_number(L, N) :- concat_number(L, 0, N).
concat_number([], A, A).
concat_number([H|T], A, N) :- A1 is A * 10 + H, concat_number(T, A1, N), !.

decimal(N, R) :- decimal(N, [], R).
decimal(0, A, A).
decimal(N, A, R) :- D is N mod 10, N1 is N div 10, decimal(N1, [D|A], R), !.

% exercise 4

select_min([E], E, []).
select_min([H|T], H, T) :- select_min(T, M, _), H =< M, !.
select_min([H|T], M, [H|R]) :- select_min(T, M, R), H > M.

select_sort([],[]).
select_sort(L, [H|T]) :- select_min(L, H, R), select_sort(R, T), !.

% exercise 5

insert([], E, [E]).
insert([H|T], E, [E,H|T]) :- E < H, !.
insert([H|T], E, [H|R]) :- E >= H, insert(T, E, R).

insert_sort([], []).
insert_sort([H|T], R) :- insert_sort(T, S), insert(S, H, R), !.

% exercise 6

reverse2(X, Y) :- reverse2(X, [], Y).
reverse2([], A, A).
reverse2([H|T], A, Y) :- reverse2(T, [H|A], Y), !.

% exercise 7

permf(X, Y) :- var(X), !, perm(Y, X).
permf(X, Y) :- perm(X, Y).


