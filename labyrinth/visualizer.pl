% Stampa il labirinto con il percorso trovato
printLabyrinthWithPath(Path) :-
    row_num(Rows), % Ottiene il numero di righe del labirinto.
    col_num(Cols), % Ottiene il numero di colonne del labirinto.
    initialPosition(Start), % Ottiene la posizione iniziale (Start).
    finalPosition(End), % Ottiene la posizione finale (End).
    findall(O, occupied(O), Occupied), % Trova tutte le posizioni occupate (Occupied).
    printRowsWithPath(1, Rows, Cols, Start, End, Occupied, Path). % Stampa tutte le righe del labirinto.

% Stampa tutte le righe del labirinto con il percorso
printRowsWithPath(CurrentRow, MaxRow, _, _, _, _, _) :- 
    CurrentRow > MaxRow, !.
printRowsWithPath(CurrentRow, MaxRow, Cols, Start, End, Occupied, Path) :-
    printRowWithPath(CurrentRow, Cols, Start, End, Occupied, Path), % Stampa una singola riga.
    NewRow is CurrentRow + 1, % Passa alla riga successiva.
    printRowsWithPath(NewRow, MaxRow, Cols, Start, End, Occupied, Path). % Ricorsivamente stampa le righe rimanenti.

% Stampa una singola riga del labirinto con il percorso
printRowWithPath(Row, Cols, Start, End, Occupied, Path) :-
    printColumnsWithPath(Row, 1, Cols, Start, End, Occupied, Path), % Stampa tutte le colonne della riga.
    nl. % Va a capo dopo aver stampato la riga.

% Stampa tutte le colonne di una riga con il percorso
printColumnsWithPath(_, CurrentCol, MaxCol, _, _, _, _) :- 
    CurrentCol > MaxCol, !.
printColumnsWithPath(Row, CurrentCol, MaxCol, Start, End, Occupied, Path) :-
    (Start = pos(Row, CurrentCol) -> write('S') ; % Se la posizione è quella iniziale, stampa 'S'.
    End = pos(Row, CurrentCol) -> write('E') ;   % Se la posizione è quella finale, stampa 'E'.
    member(pos(Row, CurrentCol), Path) -> write('*') ; % Se la posizione fa parte del percorso, stampa '*'.
    member(pos(Row, CurrentCol), Occupied) -> write('#') ; % Se la posizione è occupata, stampa '#'.
    write('.')), % Altrimenti, stampa '.' per una posizione libera.
    NewCol is CurrentCol + 1, % Passa alla colonna successiva.
    printColumnsWithPath(Row, NewCol, MaxCol, Start, End, Occupied, Path). % Ricorsivamente stampa le colonne rimanenti.