% #################################################
% Il predicato printFrontier/1 stampa le informazioni
% dei nodi presenti nella lista di input.
% #################################################
printFrontier([]). % Caso base: se la lista è vuota, non fa nulla.
printFrontier([node(S, ActionsListForS, ActualPathCost, HeuristicCost)|Frontier]) :-
  % Calcola il costo totale F come somma del costo del percorso attuale e del costo euristico.
  H is ActualPathCost + HeuristicCost,
  % Stampa le informazioni del nodo corrente.
  write("\n|Node: "), write(S), write(" | F: "), write(H),
  write("\n| - Actions performed to reach current state: "), write(ActionsListForS),
  write("\n| - Actual path cost "), write(ActualPathCost),
  write("\n| - Heuristic cost: "), write(HeuristicCost),
  write("\n|"),
  % Richiama ricorsivamente la funzione per stampare il resto della lista.
  printFrontier(Frontier).

% #################################################
% Il predicato printFrontierWithBound/1 stampa le 
% informazioni dei nodi presenti nella lista di input,
% includendo anche la profondità del nodo.
% #################################################
printFrontierWithBound([]). % Caso base: se la lista è vuota, non fa nulla.
printFrontierWithBound([node(S, ActionsListForS, ActualPathCost, HeuristicCost, DepthOfS)|Frontier]) :-
  % Calcola il costo totale F come somma del costo del percorso attuale e del costo euristico.
  H is ActualPathCost + HeuristicCost,
  % Stampa le informazioni del nodo corrente, inclusa la profondità.
  write("\n|Node: "), write(S), write(" | F: "), write(H),
  write("\n| - Actions performed to reach current state: "), write(ActionsListForS),
  write("\n| - Actual path cost "), write(ActualPathCost),
  write("\n| - Heuristic cost: "), write(HeuristicCost),
  write("\n| - Depth: "), write(DepthOfS),
  write("\n|"),
  % Richiama ricorsivamente la funzione per stampare il resto della lista.
  printFrontierWithBound(Frontier).

% #################################################
% Il predicato setElement/1 sostituisce un elemento
% in una lista alla posizione specificata.
% #################################################
setElement([_|Tail], 0, X, [X|Tail]):-!. % Caso base: sostituisce l'elemento alla posizione 0.
setElement([Head|Tail], Pos, X, [Head|NuovaTail]):-
  % Calcola la nuova posizione decrementando Pos di 1.
  Pos1 is Pos-1,
  % Richiama ricorsivamente la funzione per modificare la lista.
  setElement(Tail, Pos1, X, NuovaTail).

% #################################################
% Il predicato a_star_comparator/3 fornisce un 
% comparatore per ordinare la frontiera dell'A* 
% dopo ogni nuova espansione.
% #################################################
a_star_comparator(R, node(_, _, G1, H1), node(_, _, G2, H2)) :-
  F1 is G1 + H1,
  F2 is G2 + H2,
  ( F1 < F2 -> R = (<)
  ; F1 > F2 -> R = (>)
  ; R = (=)
  ).

