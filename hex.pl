% Define the size of the board
board_size(11).
% size(L) = read(L).

% players definitions
black(X) :- X = '\u2b22'.
white(X) :- X = '\u2b21'.

% Define the board
createBoard(B) :-
    B = [['.','.','.'],
         ['.','.','.'],
         ['.','.','.']].


printBoard([]) :- nl.
printBoard([H|T]) :-
    printLine(H),
    printBoard(T).

% TODO - implement this
% play(Board, Player, NewBoard) :-

printLine([]) :-
    nl.
printLine([H|T]) :-
    write(H),write(' '),
    printLine(T).

% ?- createBoard(B), printBoard(B).

printTab(Space) :-
    atom_concat(' ', Space, NewSpace).

