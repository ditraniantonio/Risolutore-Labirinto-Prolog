% Carica i moduli necessari per inizializzare il labirinto e le utility
:- ['./labyrinth/loader.pl', 'utils.pl'].

% Usa un predicato dinamico per tenere traccia del numero di nodi visitati
:- dynamic counter/1.
counter(0). % Inizializza il contatore a 0

% Incrementa il contatore ogni volta che visitiamo un nodo
inc_counter :-
  retract(counter(N)), % Rimuove il valore corrente del contatore
  N1 is N + 1,         % Incrementa il valore del contatore
  assert(counter(N1)). % Aggiorna il contatore con il nuovo valore

% Resetta il contatore a 0 prima di iniziare una nuova ricerca
reset_counter :-
  retractall(counter(_)), % Rimuove tutti i valori del contatore
  assert(counter(0)).     % Reimposta il contatore a 0

% ###################################################
% Iterative Deepening (ID) con percorso, azioni e inferenze
% ###################################################

% Punto di ingresso della ricerca
start :-
  reset_counter,          % Resetta il contatore prima di iniziare
  id(Path, Actions),      % Avvia la ricerca iterativa
  printLabyrinthWithPath(Path), nl, % Stampa il labirinto con il percorso trovato
  counter(C),             % Recupera il numero di nodi visitati
  write('Percorso: '), nl, write(Path), nl, % Stampa il percorso trovato
  write('Azioni: '), nl, write(Actions), nl, % Stampa le azioni effettuate
  write('Nodi visitati: '), write(C), nl. % Stampa il numero di nodi visitati

% id(-Path, -Actions)
% Esegue ricerca iterativa fino alla profondità massima
id(Path, Actions) :-
  maxDepth(D),            % Recupera la profondità massima consentita
  initialPosition(S),     % Recupera la posizione iniziale
  length(_, L),           % Genera una lista di lunghezza L
  L =< D,                 % Verifica che la profondità corrente sia entro il limite
  write("profondita: "), write(L), write("\n"), nl, % Stampa la profondità corrente
  id_search(S, [], [S], L, Path, Actions), % Avvia la ricerca con profondità L
  !.                      % Termina la ricerca al primo risultato trovato

% Caso base: stato finale raggiunto
id_search(S, Actions, Path, _, FinalPath, FinalActions) :-
  inc_counter,                       % Incrementa il contatore per il nodo corrente
  finalPosition(S),                  % Verifica se lo stato corrente è quello finale
  reverse(Path, FinalPath),          % Inverte il percorso per ottenere l'ordine corretto
  reverse(Actions, FinalActions).    % Inverte le azioni per ottenere l'ordine corretto

% Caso ricorsivo: esplora nuove mosse se c'è profondità
id_search(S, Actions, Path, N, FinalPath, FinalActions) :-
  N > 0,                             % Verifica che ci sia ancora profondità disponibile
  inc_counter,                       % Incrementa il contatore per il nodo corrente
  allowed(Dir, S),                   % Recupera una direzione valida per lo stato corrente
  move(Dir, S, NewS),                % Calcola il nuovo stato dopo il movimento
  \+ member(NewS, Path),             % Verifica che il nuovo stato non sia già nel percorso
  N1 is N - 1,                       % Riduce la profondità disponibile
  id_search(NewS, [Dir | Actions], [NewS | Path], N1, FinalPath, FinalActions). % Continua la ricerca
