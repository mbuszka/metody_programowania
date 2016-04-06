% exercise 1

appn(L, R) :-
    appn(L, AT-AT, R).

appn([], A-AT, A) :-
    AT = [].
appn([H|T], A-AT, R) :-
    appn_(H, A-AT, AppT),
    appn(T, A-AppT, R).

appn_([], _-ResT, ResT).
appn_([H|T], A-AT, ResT) :-
    AT = [H|AppT],
    appn_(T, A-AppT, ResT).

% exercise 2

flatten(T, L) :- flatten(T, AT-AT, L-[]).
flatten(leaf, A-AT, A-AT).
flatten(node(L, V, R), A-AT, A-RT) :-
    flatten(L, A-AT, A-[V|LT]),
    flatten(R, A-LT, A-RT).

% exercise 3

halve(X, L, R) :-
    halve(X, X, L, R),
    !.
halve(X, [], [], X).
halve(X, [_| []], [], X).
halve([H| T], [_, _| RT], [H|L], R) :-
    halve(T, RT, L, R).

% exercise 4

merge(X, Y, R) :- merge(X, Y, AT-AT, R).
merge([], Y, A-Y, A).
merge(X, [], A-X, A).
merge([H1| X], [H2| Y], A-AT, R) :-
    (H1 =< H2, AT = [H1| RT], merge(X, [H2| Y], A-RT, R) ;
     AT = [H2| RT], merge([H1| X], Y, A-RT, R)),
    !.


mergeSort([X], [X]).
mergeSort(List, Res) :-
    halve(List, L, R),
    mergeSort(L, LS),
    mergeSort(R, RS),
    merge(LS, RS, Res),
    !.

% exercise 5

tail(0, X, X).
tail(N, [_|T], R) :-
    M is N - 1,
    tail(M, T, R).

mergeSort2([X], _, [X]).
mergeSort2(List, N, Res) :-
    N2 is N div 2,
    M is N - N2,
    tail(N2, List, Rest),
    mergeSort2(List, N2, L),
    mergeSort2(Rest, M, R),
    merge(L, R, Res).

% exercise 6

singles([], []).
singles([H|T], [[H]| S]) :-
    singles(T, S).

step([], []).
step([X], [X]).
step([L1, L2|T], [M|LM]) :-
    merge(L1, L2, M),
    step(T, LM).


mergeRec([L], L).
mergeRec(LL, R) :-
    step(LL, LM),
    mergeRec(LM, R).

mergeSort3(L, S) :-
    singles(L, Split),
    mergeRec(Split, S),
    !.


% exercise 7

split([], _, [], []).
split([H|T], Med, Small, Large) :-
    (H < Med, Small = [H|S], Large = L ;
     H >= Med, Small = S, Large = [H|L]),
    split(T, Med, S, L).


qsort(L, R) :-
    qsort(L, [], R).
qsort([], A, A).
qsort([H|T], A, R) :-
    split(T, H, Small, Large),
    qsort(Large, A, LS),
    qsort(Small, [H|LS], R).

% exercise 8

is_divided([], _) :-
  fail.
is_divided([H|_], N) :-
  M is N mod H,
  M == 0,
  !.
is_divided([_|T], N) :-
  is_divided(T, N),
  !.

wrap(PS-T, C) :-
  T = [],
  is_divided(PS, C).

sieve(N, N, PS-[]) :-
  not(is_divided(PS, N)).
sieve(N, C, PS-T) :-
  (not(wrap(PS-T, C)), T = [C|TS] ;
   T = TS),
  D is C + 1,
  sieve(N, D, PS-TS).

prime(N) :-
  sieve(N, 2, AT-AT).
