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
    write(H), write(' '),
    printLine(T).

% ?- createBoard(B), printBoard(B).

printTab(Space) :-
    atom_concat(' ', Space, NewSpace).

% test with piramid -------------------------
print_pyramid(N) :-
    print_pyramid(N, N).

print_pyramid(N, M) :-
    M > 0,
    print_spaces(M),
    Stars is 2 * (N - M) + 1,
    print_stars(Stars),
    nl,
    NewM is M - 1,
    print_pyramid(N, NewM).
print_pyramid(_, 0).

print_spaces(N) :-
    N > 0,
    write(' '),
    NewN is N - 1,
    print_spaces(NewN).
print_spaces(0).

print_stars(N) :-
    N > 0,
    write('*'),
    NewN is N - 1,
    print_stars(NewN).
print_stars(0).
