/* Funktory do budowania klauzul */

:- op(200, fx, ~).
:- op(500, xfy, v).

/* Główny program: main/1. Argumentem jest atom będący nazwą pliku
 * z zadaniem. Przykład uruchomienia:
 *    ?- main('zad125.txt').
 * Plik z zadaniem powinien być plikiem tekstowym, na którego
 * początku znajduje się lista klauzul zbudowanych za pomocą funktorów
 * v/2 i ~/1 (szczegóły znajdują się w opisie zadania). Listę zapisujemy
 * w notacji prologowej, tj. rozdzielając elementy przecinkami
 * i otaczając listę nawiasami [ i ]. Za nawiasem ] należy postawić
 * kropkę. Wszelkie inne znaki umieszczone w pliku są pomijane przez
 * program (można tam umieścić np. komentarz do zadania).
 */

main(FileName) :-
   readClauses(FileName, Clauses),
   prove(Clauses, Proof),
   writeProof(Proof).

/* Silnik programu: predykat prove/2 — do napisania w ramach zadania.
 * Predykat umieszczony poniżej nie rozwiązuje zadania. Najpierw
 * wypisuje klauzule wczytane z pliku, a po nawrocie przykładowy dowód
 * jednego z zadań. Dziewięć wierszy następujących po tym komentarzu
 * należy zastąpić własnym rozwiązaniem. */

/*
(literals count, negative_literals, positive_literals)
convert_clause(p v q v ~r , cl()) converts input clause format into internal representation
state(Count, Clauses, Axioms)

*/

%% use_library(library(ordsets)).
%% use_library(library(assoc)).

%% zipWith(_, _, [], []).
%% zipWith(_, [], _, []).
%% zipWith(Fun, [H1|T1], [H2|T2], [R|Zipped]) :-
%%     call(Fun, H1, H2, R),
%%     zipWith(Fun, T1, T2, Zipped).

%% fillNr(cl(C, N, P), Nr, cl(C, N, P)-Nr).

%% convertClause(~X v R, cl(N, Neg, P)) :-
%%     convertClause(R, cl(N1, T, P)),
%%     ord_add_element(T, X, Neg),
%%     N is N1 + 1, !.
%% convertClause(X v R, cl(N, Neg, Pos)) :-
%%     convertClause(R, cl(N1, Neg, P)),
%%     ord_add_element(P, X, Pos),
%%     N is N1 + 1, !.
%% convertClause(~X, cl(1, [X], [])) :- !.
%% convertClause(X, cl(1, [], [X])) :- !.
%% convertClause([], cl(0, [], [])) :- !.

%% neg(X, ~X).

%% formatClause(cl(0, [], []), Par1, Par2, ([], (Par1, Par2))).
%% formatClause(cl(_, N, P), Par1, Par2, (Res, (Par1, Par2))) :-
%%     maplist(neg, N, Neg),
%%     append(P, Neg, L),
%%     format_(L, Res).

%% format_([X], X).
%% format_([H|T], H v L) :-
%%     format_(T, L).

%% formatAxiom(C,(Res, axiom)) :-
%%     formatClause(C, 1, 1, (Res,_)).

%% resolve(cl(_, N1, P1), cl(_, N2, P2), cl(C3, N3, P3)) :-
%%     select(X, N1, Rneg1), select(X, P2, Rpos2),
%%     !,
%%     ord_union(Rneg1, N2, N3),
%%     ord_union(P1, Rpos2, P3),
%%     length(N3, L1), length(P3, L2),
%%     C3 is L1 + L2.
%% resolve(cl(_, N1, P1), cl(_, N2, P2), cl(C3, N3, P3)) :-
%%     select(X, P1, Rpos1), select(X, N2, Rneg2),
%%     !,
%%     ord_union(Rneg2, N1, N3),
%%     ord_union(P2, Rpos1, P3),
%%     length(N3, L1), length(P3, L2),
%%     C3 is L1 + L2.

%% reason(Clauses, NewClauses, Counter, Res) :-
%%     gen_assoc(C, Clauses, Nr1),
%%     gen_assoc(D, Clauses, Nr2),
%%     C \== D,
%%     resolve(C, D, R),
%%     not(get_assoc(R, Clauses, _)),
%%     formatClause(R, Nr1, Nr2, Res),
%%     put_assoc(R, Clauses, Counter, NewClauses).

%% prove_(Clauses, _, []) :-
%%     get_assoc(cl(0, [], []), Clauses, _).
%% prove_(Clauses, Counter, [Res|Proof]) :-
%%     reason(Clauses, NewClauses, Counter, Res),
%%     !,
%%     NCounter is Counter + 1,
%%     prove_(NewClauses, NCounter, Proof).


%% prove(Strings, FullProof) :-
%%     length(Strings, Len),
%%     numlist(1, Len, Idxs),
%%     maplist(convertClause, Strings, C),
%%     sort(C, Cls),
%%     zipWith(fillNr, Cls, Idxs, Pairs),
%%     maplist(formatAxiom, Cls, Axioms),
%%     Counter is Len + 1,
%%     ord_list_to_assoc(Pairs, Clauses),
%%     prove_(Clauses, Counter, Proof),
%%     append(Axioms, Proof, FullProof),
%%     !.
%% */
/* Pozostała część pliku zawiera definicje predykatów wczytujących listę
 * klauzul i wypisujących rozwiązanie. Wykorzystane predykaty
 * biblioteczne SWI-Prologu (wersja kompilatora: 6.6.6):
 *
 *    close/1
 *    format/2
 *    length/2
 *    maplist/3
 *    max_list/2
 *    nl/0
 *    open/3
 *    read/2
 *    write_length/3
 *
 * Dokumentację tych predykatów można uzyskać wpisując powyższe napisy
 * na końcu następującego URL-a w przeglądarce WWW:
 *    http://www.swi-prolog.org/pldoc/doc_for?object=
 * np.
 *    http://www.swi-prolog.org/pldoc/doc_for?object=write_length/3
 * lub jako argument predykatu help/1 w konsoli interpretera SWI
 * Prologu, np.
 *    ?- help(write_length/3).
 */

readClauses(FileName, Clauses) :-
   open(FileName, read, Fd),
   read(Fd, Clauses),
   close(Fd).

/* Wypisywanie dowodu */

writeProof(Proof) :-
   maplist(clause_width, Proof, Sizes),
   max_list(Sizes, ClauseWidth),
   length(Proof, MaxNum),
   write_length(MaxNum, NumWidth, []),
   nl,
   writeClauses(Proof, 1, NumWidth, ClauseWidth),
   nl.

clause_width((Clause, _), Size) :-
   write_length(Clause, Size, []).

writeClauses([], _, _, _).
writeClauses([(Clause,Origin) | Clauses], Num, NumWidth, ClauseWidth) :-
   format('~t~d~*|.  ~|~w~t~*+  (~w)~n',
          [Num, NumWidth, Clause, ClauseWidth, Origin]),
   Num1 is Num + 1,
   writeClauses(Clauses, Num1, NumWidth, ClauseWidth).

/* twi 2016/03/13 vim: set filetype=prolog fileencoding=utf-8 : */
