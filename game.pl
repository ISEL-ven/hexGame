:- module(game, [start_game/2]).

:- use_module(options).
:- use_module(board).

% ***************************************
%               GAME
% ***************************************

% TODO: implement checkVictory()
% TODO: implement moves() usando o find-all

%
% GAME MODES
%

% Mode 1 - Player vs Player
start_game(1, Size) :- 
    print_dialog('GAME STARTED'),
    write('To play a piece write (e.g "a1")\n'),
    create_board(Size, Board),                  % Create the board based on it's size
    game_loop(Board, 'WHITE').                  % Start the game 

% TODO: Mode 2 - Player vs CPU
start_game(2, Size) :- 
    nl.

% TODO: Mode 3 - Player vs CPU
start_game(2, Size) :- 
    nl.

%
% GAME LOOP
%

game_loop(Board, Player) :-
    print_board(Board),
    %check if game is over TODO
    %if not over
    write(Player), write(' to move'),
    get_move(Board, Move),                      % Read the move from input and validate it
    apply_move(Move, Player, Board, NewBoard),  % Apply the move to the logic board
    next_player(Player, NextPlayer),            % Switch to the next player
    game_loop(NewBoard, NextPlayer).            

%
% MOVES
%

% Reads input, check string format and if move is valid on board
get_move(Board, Move) :-
    repeat,  
        read(Input),
        (
            validate_input(Input),              % Check if input is formatted
            extract_coords(Input, Move),        % Convert input to Move [X, Y]
            validate_move(Move, Board), !       % Validate if move is valid
            ;   % OR
            fail                                % Else: Re-read new input
        ).

% Apply move to Board
apply_move(Move, Player, Board, NewBoard) :-
    piece(Player, Piece),
    modify_board(Move, Board, Piece, NewBoard).

% TODO check if input is type letterNumber - ex a1 or c3 - if not read again
% TODO Also validate if input doesnt overreach board limits based on Board.Length
validate_input(Input) :- 
    nl.
     
%  Extracts the coords from the input (e.g: d3 -> X = 4, Y = 3)
extract_coords(Input, Move) :- 
    sub_string(Input, 0, 1, _, XLetter),        % Get first position (X) from input
    letter_to_number(XLetter, X),               % Convert X(letter) to number    
    sub_string(Input, 1, 1, _, YAtom),          % Get second position (Y) from input
    atom_number(YAtom, Y),                      % Convert Y(Atom) to number     
    Move = [X, Y].                                       

letter_to_number(Letter, Number) :-
    atom_codes(Letter, [Code]),
    Number is Code - 96.

% TODO check if move is valid (hex position must be empty to play)
validate_move(Move, Board) :-
    nl.

% ***************************************
%               PLAYERS
% ***************************************   

% Get players pieces chars
piece('BLACK', '\u2b21'). % ⬢
piece('WHITE', '\u2b22'). % ⬡

% Get next player
next_player('BLACK', 'WHITE').
next_player('WHITE', 'BLACK').
     