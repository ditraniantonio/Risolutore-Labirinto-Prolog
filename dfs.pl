% Carica i moduli necessari per inizializzare il labirinto e le utility
:- ['./labyrinth/loader.pl', 'utils.pl'].

% ###################################################
% Depth-First Search (DFS) con percorso e azioni e contatore di nodi visitati
% ###################################################

:- dynamic counter/1.
counter(0).

reset_counter :-
  retractall(counter(_)),
  assert(counter(0)).

inc_counter :-
  retract(counter(N)),
  N1 is N + 1,
  assert(counter(N1)).

% Punto di ingresso della ricerca: avvia dfs/2 e stampa i risultati
start :-
  reset_counter,          % Resetta il contatore prima di iniziare
  dfs(Path, Actions),
  printLabyrinthWithPath(Path), nl, % Stampa il labirinto con il percorso trovato
  counter(C),
  % Stampa il percorso e le azioni trovate
  write('Percorso: '), write(Path), nl,
  write('Azioni: '), write(Actions), nl,
  write('Nodi visitati: '), write(C), nl. % Stampa il numero di nodi visitati.

% dfs(-Path, -Actions)
% Inizializza lo stato iniziale e avvia la ricerca in profondità.
% La pila iniziale contiene un solo nodo:
%   - stato iniziale
%   - lista vuota di azioni
%   - lista contenente solo lo stato iniziale come percorso
dfs(Path, Actions) :-
  initialPosition(S),
  dfs_search([node(S, [], [S])], [], Path, Actions).

% Caso base della DFS:
% se il primo nodo nella pila è uno stato finale,
% abbiamo trovato la soluzione. Invertiamo le liste per ottenere l’ordine corretto.
dfs_search([node(State, Actions, Path) | _], _, FinalPath, FinalActions) :-
  inc_counter,                       % Incrementa il contatore per il nodo corrente
  finalPosition(State),             % Verifica se lo stato corrente è quello finale
  reverse(Path, FinalPath),         % Percorso ordinato
  reverse(Actions, FinalActions).   % Azioni ordinate

% Caso ricorsivo:
% Espande il primo nodo nella pila e genera tutti i successori validi.
dfs_search([node(State, Actions, Path) | Others], Visited, FinalPath, FinalActions) :-
  inc_counter,                       % Incrementa il contatore per il nodo corrente
  % Trova tutte le mosse valide da "State" usando allowed/2 e move/3.
  % Ogni nuova mossa genera un nuovo nodo DFS.
  findall(
    node(NewState, [Dir | Actions], [NewState | Path]),
    (
      allowed(Dir, State),                % Direzione valida
      move(Dir, State, NewState),        % Applica la mossa
      \+ member(NewState, Path),         % Evita cicli
      \+ member(NewState, Visited)       % Evita rivisitazioni
    ),
    Successors
  ),
  % Inserisce i successori IN TESTA alla pila (DFS → LIFO)
  append(Successors, Others, UpdatedStack),
  % Continua la ricerca con la pila aggiornata
  dfs_search(UpdatedStack, [State | Visited], FinalPath, FinalActions).
