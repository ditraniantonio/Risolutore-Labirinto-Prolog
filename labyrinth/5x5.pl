% 5x5 labyrinth
:- dynamic initialPosition/1, finalPosition/1, occupied/1, row_num/1, col_num/1.


row_num(5).
col_num(5).

initialPosition(pos(2,2)).
finalPosition(pos(5,4)).

occupied(pos(1,1)).
occupied(pos(1,2)).

occupied(pos(2,4)).

occupied(pos(3,2)).
occupied(pos(3,3)).