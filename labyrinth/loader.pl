%###################################################
% Per utilizzare diverse configurazioni di dominio, cambia il
% primo parametro del loader qui sotto, cioe 5x5_A.pl, 10x10_B.pl, etc.
%
% examples of labyrinths possibile configuration:
%   • :- ['5x5.pl', 'actions.pl', 'heuristic.pl'].
%   • :- ['10x10.pl', 'actions.pl', 'heuristic.pl'].
%###################################################
:- ['5x5.pl', 'actions.pl', 'heuristic.pl', 'visualizer.pl']. % Caricamento della configurazione del labirinto 

% ogni configurazione del labirinto ha un file .pl dedicato, ognuna con le proprie dimensioni e ostacoli
% Per caricare un labirinto diverso, basta cambiare il nome del file qui sopra
%###################################################