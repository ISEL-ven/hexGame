%
% MAIN 
%
main(Size) :- 
    write('##############\n## HEX GAME ##\n##############\n\n'),
    create_board(Size, Board),
    print_board(Size, Board).

%
% BOARD
%

% create_fixed_board(B) :-
%     B = [['.','.','.'],
%          ['.','.','.'],
%          ['.','.','.']].

% Creates dynamic grid - solution from chatgpt
create_board(Size, Board) :-
    length(Board, Size),
    create_rows(Size, Board).

create_rows(_, []).
create_rows(Size, [Row|Rows]) :-
    length(Row, Size),
    maplist(=(.), Row),
    create_rows(Size, Rows).

% prints board with coords labels
print_board(Size, Board) :-
    print_letter_coords(Size),
    print_grid(Board, ' ', 1),
    print_letter_coords(Size),
    nl.

% prints X axis letter coords
print_letter_coords(Size) :-
    write('  '),
    char_code('A', A_code),                  % get the code of char 'A'
    LastCharCode is A_code + Size - 1,       % get the code of char 'A' + Size
    print_line_helper(A_code, LastCharCode).

print_line_helper(N, LastCharCode) :-
    N > LastCharCode, nl, !.
print_line_helper(N, LastCharCode) :-
    char_code(Char, N),                      % get the char of code N
    write(Char),
    write(' '),
    N1 is N + 1,
    print_line_helper(N1, LastCharCode).

% prints grid AND Y numbers coords
print_grid([], Space, NumCoord) :-
    print_tab(Space, NewSpace).
print_grid([H|T], Space, NumCoord) :-
    print_tab(Space, NewSpace),
    print_num_coord(NumCoord),
    print_line(H),
    print_num_coord(NumCoord),
    nl,
    NextNumCoord is NumCoord + 1,
    print_grid(T, NewSpace, NextNumCoord).

print_tab(Space, NewSpace) :-
    write(Space),
    atom_concat(' ', Space, NewSpace).

print_num_coord(NumCoord) :-
    write(NumCoord),
    write(' ').

print_line([]).
print_line([H|T]) :-
    write(H), 
    write(' '),
    print_line(T).

% get elem
getElem_nth1(Board, RowIndex, ColIndex, Elem) :-
    nth1(RowIndex, Board, Row),
    nth1(ColIndex, Row, Elem).

%
% PLAYERS
%   

% players definitions
player(black) :- '\u2b22'.
player(white) :- '\u2b21'.
%black(X) :- X = '\u2b22'.
%white(X) :- X = '\u2b21'.

% next player
next_player(black, white).
next_player(white, black).

% TODO - implement this
% play(Board, Player, NewBoard) :-

% TODO: implement checkVictory()

% TODO: implement moves() usando o find-all

% DEBUG AND TESTS ---------------------------------------------
% ?- main(3).
% ?- createBoard(B), printBoard(B, ' ').


