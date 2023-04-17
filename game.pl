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
    write('To play a piece write (e.g "a/1")\n'),
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

% Reads Input and returns a Move if is valid. e.g. Input = b/3 -> Move = [2,3]
get_move(Board, Move) :-
    repeat,  
        read(Input),
        (
            convert_input(Input, Move),         % Convert input into a Move
            validate_move(Move, Board), !       % Validate if Move is valid to play
            ;   % OR
            write('Invalid move.\n'),
            fail                                % Else: Re-read new input
        ).

% Apply move to Board
apply_move(Move, Player, Board, NewBoard) :-
    piece(Player, Piece),
    modify_board(Move, Board, Piece, NewBoard).

convert_input(XLetter/Y, Move) :-
    nonvar(XLetter),                            % Check if XLetter is not capitalized/a variable
    atom_codes(XLetter, [XCode]),               % Get code of X Letter
    X is XCode - 96,                            % Get X as Number. e.g. a = 1
    Move = [X,Y].                         

% TODO position is empty part not done
validate_move([X,Y], Board) :-
    length(Board, Board_Length),                % Get Board length
    X >= 1, X =< Board_Length,                  % Check X is on Board limits
    Y >= 1, Y =< Board_Length.                  % Check Y is on Board limits

% ***************************************
%               PLAYERS
% ***************************************   

% Get players pieces chars
piece('BLACK', '\u2b21'). % ⬢
piece('WHITE', '\u2b22'). % ⬡

% Get next player
next_player('BLACK', 'WHITE').
next_player('WHITE', 'BLACK').
     