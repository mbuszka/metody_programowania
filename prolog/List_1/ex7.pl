%colour\2.
%lives_in\2.
%drinks\2.
%smokes\2.
%has\2.
%neighbour\2.

%lives_in(englishman,X) :- colour(X,red).
%has(spaniard,dog).
%lives_in(X,Y) :- colour(Y,green), drinks(X,coffe).
%drinks(ukrainian,coffe).
%neighbour(X,Y) :- colour(X,green), colour(Y,white).

neighbour(X,Y) :- X=1, Y=2.
neighbour(X,Y) :- X=2, Y=3.
neighbour(X,Y) :- X=3, Y=4.
neighbour(X,Y) :- X=4, Y=5.
neighbour(X,Y) :- neighbour(Y,X).
neighbour(X,Y) :- house(X,_,green,_,_,_), house(Y,_,white,_,_,_).
neighbour(X,Y) :- house(X,_,_,_,_,chesterfield), house(Y,_,_,fox,_,_).
neighbour(X,Y) :- house(X,_,_,_,_,kool), house(Y,_,_,horse,_,_).
neighbour(X,Y) :- house(X,_,_,_,_,kool), house(Y,_,_,horse,_,_).
neighbour(X,Y) :- house(X,_,blue,_,_,_), house(Y,norman,_,_,_,_).

% nr, person, colour, animal, drink, smoke
house(_,englishman,red,_,_,_).
house(_,spaniard,_,dog,_,_).
house(1,norman,_,_,_,_).
house(_,_,blue,_,_,_).
house(3,_,_,_,milk,_).
house(_,_,green,_,coffe,_).
house(_,ukrainian,_,_,tea,_).
house(_,_,_,snake,_,winston).
house(_,_,yellow,_,_,kool).
house(_,_,_,_,juice,luckystrike).
house(_,japanese,_,_,_,kent).
