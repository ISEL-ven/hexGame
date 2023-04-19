:- module(game, [start_game/2]).

:- use_module(options).
:- use_module(board).

% ***************************************
%               GAME
% ***************************************

% ***************************************
% GAME MODES
%

% Mode 1 - Player vs Player
start_game(1, Size) :-                          
    print_dialog('GAME STARTED'),
    write('To play a piece write (e.g "a/1")\n'),
    create_board(Size, Board),                      % Create the board based on it's size
    game_loop(Board, 'WHITE').                      % Start the game 

% TODO: Mode 2 - Player vs CPU
start_game(2, Size) :- 
    nl.

% TODO: Mode 3 - Player vs CPU
start_game(2, Size) :- 
    nl.
% ***************************************

% ***************************************
% Game Loop
%

game_loop(Board, Player) :-
    print_board(Board),                             % print the board in the screen
    (check_victory(Board, Player) ->                % Check if the player has won
        write(Player), write(' has won!'), nl, !;   % If so, print the winner and stop the game
        write(Player), write(' to move')),          % If not, print the player to move
    get_move(Board, Move),                          % Read the move from input and validate it
    apply_move(Move, Player, Board, NewBoard),      % Apply the move to the logic board
    next_player(Player, NextPlayer),                % Switch to the next player
    game_loop(NewBoard, NextPlayer).

% ***************************************

% ***************************************
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

% ***************************************
% Check Victory
%

check_victory(Board, Player) :-
    piece(Player, Piece),                                           % Get Piece of Player corresponding player
    starting_positions(Player, Board, StartPositions),              % Get starting positions for Player
    %write('***** DEBUG check_victory *****'), write(StartPositions), nl,
    validate_victory_path(Board, Piece, StartPositions).            % Check if there is a path from any starting position to a goal position

validate_victory_path(_, _, []) :- fail.                            % If there are no starting positions, fail
validate_victory_path(Board, Piece, [Start|StartPositions]) :-      % Check if there is a path from the current starting position (Start) to a goal position
    %write('***** DEBUG inside validate_victory_path *****'), write(Start), nl,
    %write('***** DEBUG inside path exists parameters *****'), write(Board), write(Piece), write(Start), nl,
    (path_exists(Board, Piece, Start) ->                            % If there is a path from the current starting position to a goal position
        true
    ;
        %write('***** DEBUG inside validate_victory_path *****'), write('Is False'), nl,
        validate_victory_path(Board, Piece, StartPositions)         % Else: Check if there is a path from the next starting position (StartPositions) to a goal position
    ).

path_exists(Board, Player, Start) :-
    write('***** DEBUG inside path_exists *****'), write(Start), nl,
    dfs(Board, Player, Start, []).

%dfs(_, _, [], _).                                                   % If there are no more positions to check, fail
dfs(Board, Player, [Pos|Positions], Visited) :-
    write('***** Running Debgu for DFS *****'), nl,  
    piece(Player, Piece), 
    write('***** DEBUG dfs get piece *****'), write(Piece), write(NeighBoars), nl,
    (goal(piece, Pos, Board) ->
        write('***** DEBUG inside dfs *****'), nl,
        true
        ;
        write('***** DEBUG inside dfs goal was false *****'), nl,
        findall(Neighbor, (next_move(Board, Player, Pos, Neighbor), \+ member(Neighbor, Visited)), Neighbors),
        write('***** DEBUG findall: *****'), write(Neighbor), write(NeighBoars), nl,
        append(Neighbors, Visited, UpdatedVisited),
        write('***** DEBUG append: *****'), write(Neighbor), write(NeighBoars), nl,
        dfs(Board, Player, Neighbors, UpdatedVisited),      % Check if there is a path from the current position to a goal position in this branch
        write('***** DEBUG dfs current: *****'), write(Neighbor), write(NeighBoars), nl,
        %dfs(Board, Player, Positions, Visited)          % Previous branch has been checked and failed, check the next one
        %write('***** DEBUG dfs backtracking: *****'), write(Neighbor), write(NeighBoars), nl,
    ).

goal(Piece, [X, Y], Board) :-
    write('***** DEBUG goal White: *****'), write([X/Y]), nl.
    /*piece('BLACK', Piece),
    nth1(Y, Board, Row),
    nth1(X, Row, Piece),
    length(Board, Size),
    Y =:= Size,
    !.*/
goal(Piece, [X, Y], Board) :-
    write('***** DEBUG goal Black: *****'), write([X/Y]), nl.
    /*piece('WHITE', Piece),
    nth1(Y, Board, Row),
    nth1(X, Row, Piece),
    length(Board, Size),
    X =:= Size,
    !.*/
/*
goal('BLACK', [X, _], Board) :-
    length(Board, X).

goal('WHITE', [_, Y], Board) :-
    length(Board, Y).*/


next_move(Board, Player, [X, Y], [X1, Y1]) :-
    nl. %TODO implement. Validate in the neighbors if there is a valid position to find a path.


neighbors(Board, [X, Y], Neighbors) :-
    nl.    % TODO - Calculate the neighbors of the current position

% ***************************************

% ***************************************
%               PLAYERS
% 

% Get starting positions for a player
starting_positions(Player, Board, StartPositions) :-
    piece(Player, Piece),
    (Player = 'BLACK' ->
        findall([1, Y], (nth1(Y, Board, Row), nth1(1, Row, Piece)), StartPositions)     % Get all positions where the piece is in the first column (Black Player)
    ;
        findall([X, 1], (nth1(1, Board, Row), nth1(X, Row, Piece)), StartPositions)     % Get all positions where the piece is in the first row (White Player)
    ).

% Get players pieces chars
piece('BLACK', '\u2b21'). % ⬡
piece('WHITE', '\u2b22'). % ⬢

% Get next player
next_player('BLACK', 'WHITE').
next_player('WHITE', 'BLACK').

% ***************************************   