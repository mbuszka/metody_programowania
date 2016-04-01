use_library(library(ordsets)).
use_library(library(assoc)).

zipWith(_, _, [], []).
zipWith(_, [], _, []).
zipWith(Fun, [H1|T1], [H2|T2], [R|Zipped]) :-
    call(Fun, H1, H2, R),
    zipWith(Fun, T1, T2, Zipped).

fillNr(cl(C, N, P), Nr, cl(C, N, P)-Nr).

convertClause(~X v R, cl(N, Neg, P)) :-
    convertClause(R, cl(N1, T, P)),
    ord_add_element(T, X, Neg),
    N is N1 + 1, !.
convertClause(X v R, cl(N, Neg, Pos)) :-
    convertClause(R, cl(N1, Neg, P)),
    ord_add_element(P, X, Pos),
    N is N1 + 1, !.
convertClause(~X, cl(1, [X], [])) :- !.
convertClause(X, cl(1, [], [X])) :- !.
convertClause([], cl(0, [], [])) :- !.

neg(X, ~X).

formatClause(cl(0, [], []), Par1, Par2, ([], (Par1, Par2))).
formatClause(cl(_, N, P), Par1, Par2, (Res, (Par1, Par2))) :-
    maplist(neg, N, Neg),
    append(P, Neg, L),
    format_(L, Res).

format_([X], X).
format_([H|T], H v L) :-
    format_(T, L).

formatAxiom(C,(Res, axiom)) :-
    formatClause(C, 1, 1, (Res,_)).

resolve(cl(_, N1, P1), cl(_, N2, P2), cl(C3, N3, P3)) :-
    select(X, N1, Rneg1), select(X, P2, Rpos2),
    !,
    ord_union(Rneg1, N2, N3),
    ord_union(P1, Rpos2, P3),
    length(N3, L1), length(P3, L2),
    C3 is L1 + L2.
resolve(cl(_, N1, P1), cl(_, N2, P2), cl(C3, N3, P3)) :-
    select(X, P1, Rpos1), select(X, N2, Rneg2),
    !,
    ord_union(Rneg2, N1, N3),
    ord_union(P2, Rpos1, P3),
    length(N3, L1), length(P3, L2),
    C3 is L1 + L2.

reason(Clauses, NewClauses, Counter, Res) :-
    gen_assoc(C, Clauses, Nr1),
    gen_assoc(D, Clauses, Nr2),
    C \== D,
    resolve(C, D, R),
    not(get_assoc(R, Clauses, _)),
    formatClause(R, Nr1, Nr2, Res),
    put_assoc(R, Clauses, Counter, NewClauses).

prove_(Clauses, _, []) :-
    get_assoc(cl(0, [], []), Clauses, _).
prove_(Clauses, Counter, [Res|Proof]) :-
    reason(Clauses, NewClauses, Counter, Res),
    !,
    NCounter is Counter + 1,
    prove_(NewClauses, NCounter, Proof).


prove(Strings, FullProof) :-
    length(Strings, Len),
    numlist(1, Len, Idxs),
    maplist(convertClause, Strings, C),
    sort(C, Cls),
    zipWith(fillNr, Cls, Idxs, Pairs),
    maplist(formatAxiom, Cls, Axioms),
    Counter is Len + 1,
    ord_list_to_assoc(Pairs, Clauses),
    prove_(Clauses, Counter, Proof),
    append(Axioms, Proof, FullProof),
    !.
