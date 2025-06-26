:- ['./labyrinth/loader.pl', './labyrinth/heuristic.pl', './utils.pl'].

% Contatore dinamico per i nodi visitati
:- dynamic counter/1.
counter(0).

reset_counter :-
  retractall(counter(_)),
  assert(counter(0)).

inc_counter :-
  retract(counter(N)),
  N1 is N + 1,
  assert(counter(N1)).

% Punto di ingresso
start :-
  reset_counter,
  astar(Actions),
  initialPosition(S),
  build_path(S, Actions, Path),
  printLabyrinthWithPath(Path),
  counter(C),
  write('Percorso: '), write(Path), nl,
  write('Azioni: '), write(Actions), nl,
  write('Nodi visitati: '), write(C), nl.

% Inizio della ricerca A*
astar(Solution) :-
  initialPosition(S),
  heuristic(S, [], H),
  astar_search([node(S, [], 0, H)], [], Solution).

% Caso base: nodo finale raggiunto
astar_search([node(S, Actions, _, _)|_], _, Actions) :-
  inc_counter,
  finalPosition(S).

% Espansione ricorsiva
astar_search([node(S, Actions, G, H)|Frontier], Expanded, Solution) :-
  inc_counter,
  findall(Dir, allowed(Dir, S), Allowed),
  generateSons(node(S, Actions, G, H), Allowed, Expanded, Children),
  append(Children, Frontier, NewFrontier),
  predsort(a_star_comparator, NewFrontier, OrderedFrontier),
  astar_search(OrderedFrontier, [S | Expanded], Solution).

% Genera figli compatibili con nodi a 4 argomenti
generateSons(_, [], _, []).

generateSons(node(S, Actions, G, H2), [Action | Others], Expanded,
             [node(NewS, NewActions, NewG, NewH) | MoreChildren]) :-
  move(Action, S, NewS),
  \+ member(NewS, Expanded), % Assicurati che il nuovo stato non sia già espanso
  cost(S, NewS, StepCost),
  NewG is G + StepCost,
  heuristic(NewS, [], NewH),
  append(Actions, [Action], NewActions),
  generateSons(node(S, Actions, G, H2), Others, Expanded, MoreChildren),
  !.

generateSons(Node, [_ | Others], Expanded, Children) :-
  generateSons(Node, Others, Expanded, Children),
  !.

% Ricostruisce il percorso applicando le azioni partendo dallo stato iniziale
build_path(Start, [], [Start]).
build_path(Start, [Action | Rest], [Start | Tail]) :-
  move(Action, Start, Next),
  build_path(Next, Rest, Tail).
