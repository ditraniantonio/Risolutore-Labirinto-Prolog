%questo file contiene i predicati che definiscono le azioni valide nel nostro dominio.

:- dynamic col_num/1, row_num/1, occupied/1.  % predicati dinamici
% spiegazione: col_num/1 e row_num/1 e occupied/1 sono dinamiche, ciò significa che la dimensione della griglia varia in base alle configurazione scelta in loader.pl.

%#################################################
% Il predicato allowed/2 verifica se le azioni, che possono
% essere 'nord', 'sud', 'est' e 'ovest', sono fattibili
% oppure no nella posizione 'pos(R,C)'.
% La fattibilità delle azioni viene verificata testando:
%   • i limiti del labirinto
%   • e le possibili pareti del labirinto.
%#################################################
allowed(nord, pos(R,C)) :-
  R>1, 
  R1 is R-1,
  \+occupied(pos(R1,C)).

allowed(sud, pos(R,C)) :-
  \+row_num(R), % se R non è l'ultima riga(prima condizione).
  R1 is R+1,
  \+occupied(pos(R1,C)).

allowed(ovest, pos(R,C)) :-
  C>1,
  C1 is C-1,
  \+occupied(pos(R,C1)).

allowed(est, pos(R,C)) :-
  \+col_num(C),
  C1 is C+1,
  \+occupied(pos(R,C1)).

%###################################################
% Il predicato move/3 fornisce un nuovo stato modificando
% la posizione precedente 'pos(R,C)' con una nuova.
%###################################################
move(est, pos(R,C), pos(R, CAdjacent)) :-
  CAdjacent is C+1.
move(ovest, pos(R,C), pos(R, CAdjacent)) :-
  CAdjacent is C-1.
move(nord, pos(R,C), pos(RUp,C)) :-
  RUp is R-1.
move(sud, pos(R,C), pos(RDown,C)) :-
  RDown is R+1.

%###################################################
% Il predicato maxDepth/1 fornisce un limite massimo di profondità
% entro il quale la ricerca ID si fermerà.
%###################################################
maxDepth(D) :-
  row_num(R),
  col_num(L),
  D is R * L. 

%###################################################
% Il predicato cost/3 restituisce il costo di ciascuna azione
% valida nel dominio; ognuna di esse ha un costo unitario.
% È possibile modificarlo per ottenere costi diversi
% per ciascuna azione del dominio.
%###################################################
cost(pos(_,_), pos(_, _), Cost) :-
  Cost is 1.
