% Carica i moduli necessari per inizializzare il labirinto e le utility
:- ['./labyrinth/loader.pl', 'utils.pl'].

:- dynamic counter/1.
counter(0).

reset_counter :-
  retractall(counter(_)),
  assert(counter(0)).

inc_counter :-
  retract(counter(N)),
  N1 is N + 1,
  assert(counter(N1)).

% ###################################################
% Breadth-First Search (BFS) con percorso e azioni e contatore di nodi visitati.
% ###################################################

% Punto di ingresso della ricerca: avvia bfs/2 e stampa i risultati
start :-
  reset_counter,          % Resetta il contatore prima di iniziare
  bfs(Path, Actions),
  printLabyrinthWithPath(Path), nl, % Stampa il labirinto con il percorso trovato
  counter(C),             % Recupera il numero di nodi visitati
  % Stampa il percorso e le azioni trovate
  write('Percorso: '), write(Path), nl,
  write('Azioni: '), write(Actions), nl,
  write('Nodi visitati: '), write(C), nl. % Stampa il numero di nodi visitati
  
% bfs(-Path, -Actions)
% Inizializza lo stato iniziale e avvia la ricerca in ampiezza.
% La coda iniziale contiene un solo nodo:
%   - stato iniziale
%   - lista vuota di azioni
%   - lista contenente solo lo stato iniziale come percorso
bfs(Path, Actions) :-
  initialPosition(S),
  bfs_search([node(S, [], [S])], [], Path, Actions).

% Caso base della BFS:
% se il primo nodo nella coda è uno stato finale,
% abbiamo trovato la soluzione. Si restituiscono percorso e azioni,
% ma PRIMA vengono invertiti perché sono stati costruiti al contrario.
bfs_search([node(State, Actions, Path) | _], _, FinalPath, FinalActions) :-
  inc_counter,                       % Incrementa il contatore per il nodo corrente
  finalPosition(State),            % Verifica se lo stato corrente è quello finale
  reverse(Path, FinalPath),         % Percorso in ordine corretto
  reverse(Actions, FinalActions).   % Azioni in ordine corretto

% Caso ricorsivo:
% Espande il primo nodo nella coda e genera tutti i successori validi.
bfs_search([node(State, Actions, Path) | Others], Visited, FinalPath, FinalActions) :-
  inc_counter,                       % Incrementa il contatore per il nodo corrente
  % Trova tutte le mosse valide da "State" usando allowed/2 e move/3.
  % Crea nuovi nodi solo per stati non già presenti nel percorso o tra i visitati.
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
  % Aggiunge i successori in fondo alla coda (BFS → FIFO)
  append(Others, Successors, UpdatedQueue),
  % Continua la ricerca con la coda aggiornata
  bfs_search(UpdatedQueue, [State | Visited], FinalPath, FinalActions).
