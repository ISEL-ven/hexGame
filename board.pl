:- module(board, [create_board/2, modify_board/4, print_board/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               BOARD                   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Creates dynamic board
create_board(Size, Board) :-
    length(Board, Size),
    create_rows(Size, Board).
     
create_rows(_, []).
create_rows(Size, [Row|Rows]) :-
    length(Row, Size),
    maplist(=(.), Row),
    create_rows(Size, Rows).

% Modifies position [X,Y] of the Board with Char
modify_board([X, Y], Board, Char, NewBoard) :-
    nth1(X, Board, Row),
    replace(Row, Y, Char, NewRow),
    replace(Board, X, NewRow, NewBoard).

replace(List, Index, NewElement, Result) :-
    nth1(Index, List, _, Temp),
    nth1(Index, Result, NewElement, Temp).

% Prints Board with coords labels
print_board(Board) :-
    nl,
    length(Board, Size),
    print_letter_coords(Size),
    print_grid(Board, ' ', 1),
    print_letter_coords(Size),
    nl.

% Prints X axis letter Label
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

% Prints grid AND Y numbers Label
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
