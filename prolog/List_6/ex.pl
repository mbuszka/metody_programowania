  % exercise 1
test(1).
test(2).
test(3).
test(4).

sput(E, S, [E|S]).

sget([E|R], E, R).

sempty([]).

saddall(E, G, S, B) :-
  findall(E, G, B, S).

qput(E, S-[E|T], S-T).

qget([E|R]-T, E, R-T).

qempty(T-T).

qaddall(E, G, S-B, S-T) :-
  findall(E, G, B, T).

  % exercise 2

e(1,2).
e(2,3).
e(1,4).
e(3,5).
e(4,6).
e(3,6).

backtrace(Map, Start, Dest, Path) :-
  backtrace(Map, Start, Dest, [Dest], Path).
backtrace(_, S, S, A, A).
backtrace(Map, Start, Dest, Acc, Path) :-
  member(Dest-V, Map),
  backtrace(Map, Start, V, [V|Acc], Path).

dfs(Start, Dest, Path) :-
  dfs(Dest, [Start], [], [], Map),
  backtrace(Map, Start, Dest, Path).
dfs(Dest, Fringe, _, Acc, Acc) :-
  sget(Fringe, Dest, _).
dfs(Dest, Fringe, Visited, Acc, Map) :-
  sget(Fringe, V, Rest),
  \+memberchk(V, Visited),
  saddall(E, e(V,E), Rest, NewFringe),
  saddall(E-V, e(V,E), Acc, NewAcc),
  dfs(Dest, NewFringe, [V|Visited], NewAcc, Map).

f(V, E, C) :-
  e(V, E),
  \+memberchk(E, C).

bfs(Start, Dest, Path) :-
  bfs(Dest, [Start|T]-T, [], [], Map),
  backtrace(Map, Start, Dest, Path).
bfs(Dest, Fringe, _, Acc, Acc) :-
  qget(Fringe, Dest, _).
bfs(Dest, Fringe, Visited, Acc, Map) :-
  qget(Fringe, V, Rest),
  qaddall(E, f(V, E, Visited), Rest, NewFringe),
  saddall(E-V, f(V, E, Visited), Acc, NewAcc),
  bfs(Dest, NewFringe, [V|Visited], NewAcc, Map).

 % exercise 3

empty(leaf).

insert(E, leaf, node(leaf,E,leaf)) :- !.
insert(E, node(L, V, R), node(Res, V, R)) :-
  E < V, !,
  insert(E, L, Res).
insert(E, node(L, V, R), node(L, V, Res)) :-
  !,
  insert(E, R, Res).

find(_, leaf) :- fail.
find(E, node(_, E, _)) :- !.
find(E, node(L, V, R)) :-
  (E < V, T = L ; T = R), !,
  find(E, T).

findMax(_, leaf) :- fail.
findMax(M, node(_ , M, leaf)).
findMax(M, node(_ , _, R)) :-
  findMax(M, R).

delMax(_, leaf, _) :- fail.
delMax(M, node(L , M, leaf), L).
delMax(M, node(L , V, R), node(L, V, T)) :-
  delMax(M, R, T).

leftApp(leaf, leaf, leaf).
leftApp(Small, node(leaf, V, R), node(Small, V, R)).
leftApp(Small, node(L, V, R), node(Res, V, R)) :-
  leftApp(Small, L, Res).

delete(E, node(L, E, R), Res) :-
  leftApp(L, R, Res), !.
delete(E, node(L, V, R), node(Les, V, R)) :-
  E < V, !,
  delete(E, L, Les).
delete(E, node(L, V, R), node(L, V, Res)) :-
  delete(E, R, Res).
