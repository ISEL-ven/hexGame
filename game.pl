:- module(game, [start_game/2]).

:- use_module(options).
:- use_module(board).
:- use_module(minimax).

% ***************************************
%               GAME
% ***************************************

%
% GAME MODES
%

% Mode 1 - Player vs Player
start_game(1, Size) :-                          
    print_dialog('GAME STARTED'),
    write('To play a piece write (e.g "a/1")\n'),
    create_board(Size, Board),                      % Create the board based on it's size
    print_board(Board),                             % print the initial board in the screen
    game_loop(Board, 'WHITE').                      % Start the game 

% TODO: Mode 2 - Player vs CPU
start_game(2, Size) :- 
    print_dialog('GAME STARTED'),
    write('To play a piece write (e.g "a/1")\n'),
    create_board(Size, Board),                  % Create the board based on it's size
    % game_loop_cpu(Board, 'WHITE').           % Start the game
    nl.

% TODO: Mode 3 - CPU vs Player
start_game(3, Size) :- 
    print_dialog('GAME STARTED'),
    write('To play a piece write (e.g "a/1")\n'),
    create_board(Size, Board),                  % Create the board based on it's size
    % game_loop_cpu_2(Board, 'WHITE').           % Start the game
    nl.


%
% GAME LOOP
%

game_loop(Board, Player) :-
    get_move(Board, Move),                          % Read the move from input and validate it
    apply_move(Move, Player, Board, NewBoard),      % Apply the move to the logic board
    print_board(NewBoard),                             % print the board in the screen
    (check_victory(NewBoard, Player) ->                % Check if the player has won
        write(Player), write(' has won!'), nl, !;   % If so, print the winner and stop the game
        next_player(Player, NextPlayer),                % If not, switch to the next player
        write(NextPlayer), write(' to move')),          % and print the player to move
    
    game_loop(NewBoard, NextPlayer).

game_loop_cpu(Board, Player) :-
    print_board(Board),
    %check if game is over TODO
    %if not over
    write(Player), write(' to move'),
     % TODO: é preciso chamar minimax
    get_move_cpu([Board], Move, Player),         % Read the move from input and validate it
    apply_move(Move, Player, Board, NewBoard),  % Apply the move to the logic board
    next_player(Player, NextPlayer),            % Switch to the next player
    game_loop_cpu(NewBoard, NextPlayer).      

game_loop_cpu_2(Board, Player) :-
    print_board(Board),
    %check if game is over TODO
    %if not over
    write(Player), write(' to move'),
    % TODO: é preciso chamar minimax invertido
    get_move_cpu_2(Board, Move, Player),       % Read the move from input and validate it
    apply_move(Move, Player, Board, NewBoard),  % Apply the move to the logic board
    next_player(Player, NextPlayer),            % Switch to the next player
    game_loop_cpu_2(NewBoard, NextPlayer).

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

get_move_cpu(Board, Move, Player) :-
    Player == 'BLACK',
    minimax.minimax( [Player|Board], Move, Val), !
    ;  % OR
        repeat,  
            read(Input),
            (
                convert_input(Input, Move),         % Convert input into a Move
                validate_move(Move, Board), !       % Validate if Move is valid to play
                ;   % OR
                write('Invalid move.\n'),
                fail                                % Else: Re-read new input
            ).

get_move_cpu_2(Board, Move, Player) :-
    Player == 'WHITE',
    minimax.minimax([Player|Board], Move, Val), !
    ;  % OR
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

% check if choosen move is valid
validate_move([X,Y], Board) :-
    length(Board, Board_Length),                % Get Board length
    X >= 1, X =< Board_Length,                  % Check X is on Board limits
    Y >= 1, Y =< Board_Length,                  % Check Y is on Board limits
    empty_pos([X,Y], Board).                    % Check if position is empty
    
% check if piece in position [X,Y] of the Board is empty (empty = '.')
empty_pos([X, Y], Board) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Piece),
    (Piece == '.').


% ***************************************

% ***************************************
% Check Victory
%

check_victory(Board, Player) :-
    starting_positions(Player, Board, StartPositions),              % Get starting positions for Player
    %write('***** DEBUG check_victory *****'), write(StartPositions), nl,
    validate_victory_path(Board, Player, StartPositions), !.           % Check if there is a path from any starting position to a goal position

validate_victory_path(_, _, []):-fail.                            % If there are no starting positions, fail
validate_victory_path(Board, Player, [Start|StartPositions]) :-      % Check if there is a path from the current starting position (Start) to a goal position
    (path_exists(Board, Player, [Start|StartPositions]) ->                            % If there is a path from the current starting position to a goal position
        true,!
    ;
        %write('***** DEBUG inside validate_victory_path *****'), write('Is False'), nl,
        validate_victory_path(Board, Piece, StartPositions)         % Else: Check if there is a path from the next starting position (StartPositions) to a goal position
    ).

path_exists(Board, Player, [Start|StartPositions]) :-
    %write('***** DEBUG inside path_exists *****'), write(Start), nl, write(Player), nl,
    dfs(Board, Player, [Start|StartPositions], []), !.

dfs(_, _, [], _) :- write('This has failed'), nl, fail.    % If there are no more positions to check, fail
dfs(Board, Player, [Pos| Positions], Visited) :-
    %write('***** Running Debgu for DFS *****'), nl,  
    piece(Player, Piece), 
    %write('***** DEBUG dfs get piece *****'), write(Piece), nl,
    write('Before goal    '), write(Pos), nl,
    (goal(Piece, Pos, Board) ->
        %write('***** DEBUG inside dfs *****'), nl,
        true,
        write('The has won'), !
        ;
        %write('***** DEBUG inside dfs goal was false *****'), nl,
        findall(Neighbor, (next_move(Board, Player, Pos, Neighbor), \+ member(Neighbor, Visited)), Neighbors),
        %write('***** DEBUG findall: *****'), write(Neighbor), write(Neighbors), nl,
        %write('Neighbors:'), write(Neighbors), write('   Visited   '), write(Visited), write('Neighbor'), write(Neighbor), nl,
        append(Neighbors, Visited, UpdatedVisited),
        %write('***** DEBUG append: *****'), write(Neighbor), write(Neighbors), nl,
        dfs(Board, Player, Neighbors, UpdatedVisited), !     % Check if there is a path from the current position to a goal position in this branch
        ; % OR
        %write('***** DEBUG dfs current: *****'), write(Neighbor), write(Neighbors), nl,
        dfs(Board, Player, Positions, Visited)          % Previous branch has been checked and failed, check the next one
        %write('***** DEBUG dfs backtracking: *****'), write(Neighbor), write(Neighbors), nl,
    ).

goal(Piece, [X, Y], Board) :-
    Piece == '\u2b21',  % only checks for black
    %write('Inside goal for BLACK'), write(Piece), nl,
    piece('BLACK', Piece),
    nth1(Y, Board, Row),
    nth1(X, Row, BoardPiece),
    length(Board, Size),
    BoardPiece == Piece,
    X =:= Size,
    !.

goal(Piece, [X, Y], Board) :-
    Piece == '\u2b22',  % only checks for white
    %write('Inside goal for WHITE'), write(Piece), nl,
    write('Coords'), write([X/Y]), nl,
    piece('WHITE', Piece),
    nth1(X, Board, Row),
    nth1(Y, Row, BoardPiece),
    length(Board, Size),
    BoardPiece == Piece,
    Y =:= Size,
    !.

/*
goal('BLACK', [X, _], Board) :-
    length(Board, X).

goal('WHITE', [_, Y], Board) :-
    length(Board, X).
*/

next_move(Board, Player, [X, Y], [X1, Y1]) :-
    %write('*** Inside next mode  ***'), nl,
    length(Board, Size),
    between(1, Size, X1),
    between(1, Size, Y1),
    abs(X1 - X) =< 1,
    abs(Y1 - Y) =< 1,
    (X1 =:= X, Y1 =:= Y -> fail; true),
    modify_board([X1, Y1], Board, Piece, _),
    piece(Player, Piece).

neighbors(Board, [X, Y], Neighbors) :-
    nl.    % TODO - Calculate the neighbors of the current position
    % TODO
    findall([NewX, NewY],
            (
                neighbor_coords([X, Y], [NewX, NewY]),
                valid_coords(Board, [NewX, NewY]),
                piece_at(Board, [NewX, NewY], Piece)
            ),
            Neighbors).

neighbor_coords([X, Y], [NewX, NewY]) :-
    DXY = [[1, 0], [0, 1], [-1, 0], [0, -1], [1, -1], [-1, 1]],
    member([DX, DY], DXY),
    NewX is X + DX,
    NewY is Y + DY.

valid_coords(Board, [X, Y]) :-
    length(Board, Size),
    X >= 1, X =< Size,
    Y >= 1, Y =< Size.

piece_at(Board, [X, Y], Piece) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Piece).

% ***************************************
%               PLAYERS
% ***************************************   

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