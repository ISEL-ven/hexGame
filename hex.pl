% Define the size of the board
board_size(11).
% size(L) = read(L).

% players definitions
player(black) :- '\u2b22'.
player(white) :- '\u2b21'.
%black(X) :- X = '\u2b22'.
%white(X) :- X = '\u2b21'.

% next player
next_player(black, white).
next_player(white, black).

% Define the board
createBoard(B) :-
    B = [['.','.','.'],
         ['.','.','.'],
         ['.','.','.']].

printBoardHeader :-
    write('  A B C'), nl.
    
printBoard([], Space, Counter) :-
    write('2'),
    printTab(Space, NewSpace, Counter),
    printBoardHeader(),
    nl.

printBoard([H|T], Space, Counter) :-
    write('1'),
    printTab(Space, NewSpace, Counter),
    printLine(H),
    NextCounter is Counter + 1,
    printBoard(T, NewSpace, NextCounter).

getElem_nth1(Board, RowIndex, ColIndex, Elem) :-
    nth1(RowIndex, Board, Row),
    nth1(ColIndex, Row, Elem).

% TODO - implement this
% play(Board, Player, NewBoard) :-

printLine([]) :-
    nl.
printLine([H|T]) :-
    write(H), write(' '),
    printLine(T).


% TODO: implement checkVictory()

% TODO: implement moves() usando o find-all

% DEBUG AND TESTS ---------------------------------------------
% ?- createBoard(B), printBoard(B, ' ').

printTab(Space, NewSpace, Counter) :-
    write(Space),
    write(Counter),
    write(' '),
    atom_concat(' ', Space, NewSpace).

