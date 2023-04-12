% ***************************************
%                   MAIN 
% ***************************************

main() :- 
    write('##############\n## HEX GAME ##\n##############\n\n'),
    print_options(),
    read_options().
  
% ***************************************
%                   OPTIONS
% ****************************************

print_options() :- 
    write('1 - Player vs Player\n'),
    write('2 - Player vs CPU\n'),
    write('3 - CPU vs CPU\n').
    
read_options() :- 
    write('Choose option: '),
    read(Option),
    option(Option).

% Option 1 - Player vs Player
option(1) :- 
    read_board_size(Size),                  % Get the size of the board from input
    create_board(Size, Board),              % Create the board based on it's size
    write('\n### GAME STARTED ###\nTo play a piece write (e.g "a1") \n'),
    game_loop(Board, 'WHITE').              % Start the game 

% Option 2 - Player vs CPU
option(2) :-
    write('Option 2 TODO').
% Option 3 - CPU vs CPU
option(3) :- 
    write('Option 3 TODO').
option(_) :- 
    write('*** Choose a valid option! ***\n'),
    read_options().

read_board_size(Size) :-                            
    write('Set board size (max: 9): '),
    read(Size).

% ***************************************
%                 GAME
% ***************************************

game_loop(Board, Player) :-
    print_board(Board),
    %check if game is over TODO
    %if not over
    write(Player), write(' to move'),
    get_move(Input, Board, Move),
    apply_move(Move, Player, Board, NewBoard),
    next_player(Player, NextPlayer),
    game_loop(NewBoard, NextPlayer).


get_move(Input,Board, Move) :-
    repeat,  
        read(Input),
        (
            validate_input(Input),
            extract_coords(Input, Move),
            validate_move(Move, Board), !
            ;   % OR
            fail
        ).

% TODO check if input is type letterNumber - ex a1 or c3 - if not read again
validate_input(Input) :- 
    nl.

% TODO check if move is valid (hex position must be empty to play, if not user must give new input
validate_move(Move, Board) :-
    nl.

% TODO apply move to Board
apply_move(Move, Player, Board, NewBoard) :-
    piece(Player, Piece),
    modify_board(Move, Board, Piece, NewBoard).

    
% ***************************************
%  Extract the coords from the input (e.g d3 -> X = 4, Y = 3)
% ***************************************
extract_coords(Input, Move) :- 
    sub_string(Input, 0, 1, _, XLetter),    % Get first position (X) from input
    letter_to_number(XLetter, X),           % Convert X(letter) to number    
    sub_string(Input, 1, 1, _, YAtom),      % Get second position (Y) from input
    atom_number(YAtom, Y),                  % Convert Y(Atom) to number     
    Move = [X, Y].                                       

letter_to_number(Letter, Number) :-
    atom_codes(Letter, [Code]),
    Number is Code - 96.

% ***************************************
%               BOARD
% ***************************************

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
print_board(Board) :-
    nl,
    length(Board, Size),
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

% Create a NewBoard with the position [X,Y] modified with Char
modify_board([X, Y], Board, Char, NewBoard) :-
    nth1(Y, Board, Row),
    replace(Row, X, Char, NewRow),
    replace(Board, Y, NewRow, NewBoard).

replace(List, Index, NewElement, Result) :-
    nth1(Index, List, _, Temp),
    nth1(Index, Result, NewElement, Temp).

% ***************************************
%               PLAYERS
% ***************************************   

% get players pieces chars
piece('BLACK', '\u2b21').
piece('WHITE', '\u2b22').

% get next player
next_player('BLACK', 'WHITE').
next_player('WHITE', 'BLACK').

% TODO - implement this
% TODO: implement checkVictory()
% TODO: implement moves() usando o find-all

% DEBUG AND TESTS ---------------------------------------------
% ?- main().
% ?- createBoard(B), printBoard(B, ' ').
