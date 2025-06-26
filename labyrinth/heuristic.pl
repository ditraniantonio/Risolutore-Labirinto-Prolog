% questo file contiene il predicato utilizzato per calcolare l'euristica del nostro dominio.

:- dynamic move/3, finalPosition/1, col_num/1, row_num/1, maxDepth/1.

%#################################################
% Euristica della distanza di Manhattan per il gioco del labirinto.
%
% • heuristic/3
%#################################################

heuristic(pos(X1, Y1), [], L) :- 
  finalPosition(pos(X2, Y2)), 
  L is abs(X1-X2) + abs(Y1-Y2).

  /**
   * Il predicato heuristic/3 implementa l’euristica della distanza di Manhattan:
   * Prende come primo argomento la posizione corrente dell’agente (pos(X1, Y1)).
   * Il secondo argomento ([]) è ignorato in questo caso, ma serve per compatibilità con altri domini o algoritmi.
   * Il terzo argomento (L) è il valore dell’euristica calcolata.
   * 
   * All’interno della regola:
   * - finalPosition(pos(X2, Y2)) recupera la posizione finale (obiettivo) del labirinto.
   * - L is abs(X1-X2) + abs(Y1-Y2) calcola la distanza di Manhattan tra la posizione corrente e quella finale, 
   *   cioè la somma delle distanze assolute tra le coordinate X e Y.
   * 
   * In sintesi:
   * Questo predicato restituisce la distanza di Manhattan tra la posizione attuale e quella finale, 
   * che viene usata come euristica per guidare la ricerca verso la soluzione nel labirinto.
   */