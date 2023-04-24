:- module(game, [start_game/2]).

:- use_module(options).
:- use_module(board).
:- use_module(minimax).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               GAME
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% GAME MODES
%

% Mode 1 - Player vs Player
start_game(1, Size) :-                          
    print_dialog('GAME STARTED'),
    write('To play a piece write (e.g "1/a")\n'),
    create_board(Size, Board),                      % Create the Board based on it's size
    print_board(Board),                             % Print the Board
    game_loop(Board, 'WHITE').                      % Start the game 

% TODO: Mode 2 - Player vs CPU
start_game(2, Size) :- 
    print_dialog('GAME STARTED'),
    write('To play a piece write (e.g "1/a")\n'),
    create_board(Size, Board),                  % Create the Board based on it's size
    print_board(Board),                         % Print the Board
    game_loop_cpu(Board, 'WHITE').              % Start the game

% TODO: Mode 3 - CPU vs Player
start_game(3, Size) :- 
    print_dialog('GAME STARTED'),
    write('To play a piece write (e.g "1/a")\n'),
    create_board(Size, Board),                  % Create the board based on it's size
    % game_loop_cpu_2(Board, 'WHITE').           % Start the game
    nl.

%****************************************
%           GAME LOOP
%****************************************

game_loop(Board, Player) :-
    get_move(Board, Move),                              % Read Move from input and validate it
    apply_move(Move, Player, Board, NewBoard),          % Apply Move to the logic board
    print_board(NewBoard),                              % Print Board in the screen
    (check_victory(NewBoard, Player) ->                 % If Player won -> Game Over
        write('\u2b22 \u2b21 \u2b22 '), write(Player), write(' WINS! \u2b22 \u2b21 \u2b22\n'), !
        ;   %OR
        next_player(Player, NextPlayer),                % If not, switch to the next Player
        write(NextPlayer), write(' to move'),
        game_loop(NewBoard, NextPlayer)).               % Restart loop with the new board and player

game_loop_cpu(Board, Player) :-
    (Player = 'WHITE' ->
        get_move(Board, Move), ! 
        ;   %OR 
        get_move_cpu(Board, Move), write(Move)
    ),
    apply_move(Move, Player, Board, NewBoard),          % Apply Move to the logic board
    print_board(NewBoard),                              % Print Board in the screen
    (check_victory(NewBoard, Player) ->                 % If Player won -> Game Over
        write('\u2b22 \u2b21 \u2b22 '), write(Player), write(' WINS! \u2b22 \u2b21 \u2b22\n'), !
        ;   %OR
        next_player(Player, NextPlayer),                % If not, switch to the next Player
        write(NextPlayer), write(' to move'),
        game_loop_cpu(NewBoard, NextPlayer)).               % Restart loop with the new board and player

game_loop_cpu_2(Board, Player) :- fail.

%****************************************
%               MOVES
%****************************************

% Reads Input and returns a Move if is valid. e.g. Input = 3/b -> Move = [3,b]
get_move(Board, Move) :-
    repeat,  
        read(Input),
        (
            convert_input(Input, Move),         % Convert input into a Move
            validate_move(Move, Board), !       % Validate if Move is valid to play
            ;   % OR
            write('Invalid move.\n'), fail      % Else: Re-read new input
        ).

get_move_cpu(Board, Move) :-
    minimax( Board, BestSucc, Val),
    write('BEST Succ =  '), write(BestSucc), nl,
    write('MINMAX VAL = '), write(Val), nl.
  
/* get_move_cpu_2(Board, Move, Player) :-
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
            ).    */ 

% Apply move to Board
apply_move(Move, Player, Board, NewBoard) :-
    piece(Player, Piece),
    modify_board(Move, Board, Piece, NewBoard).

convert_input(X/YLetter, Move) :-
    number(X),                                  % Check if X is a number
    nonvar(YLetter),                            % Check if YLetter is not capitalized/a variable
    atom_codes(YLetter, [YCode]),               % Get code of YLetter
    Y is YCode - 96,                            % Get Y as Number. e.g. a = 1
    Move = [X,Y].                         

% check if choosen move is valid
validate_move([X,Y], Board) :-
    length(Board, Board_Length),                % Get Board length
    X >= 1, X =< Board_Length,                  % Check X is on Board limits
    Y >= 1, Y =< Board_Length,                  % Check Y is on Board limits
    empty_pos([X,Y], Board).                    % Check if position is empty
    
% check if piece in position [X,Y] of the Board is empty (empty = '.')
empty_pos([X, Y], Board) :-
    nth1(X, Board, Row),
    nth1(Y, Row, Piece),
    (Piece == '.').


%****************************************
%           Check Victory - DFS
%****************************************

check_victory(Board, Player) :-
    start_positions(Board, Player, StartPositions),             % Get start positions for Player
    dfs_each_start_pos(Board, Player, StartPositions).          % Check if there is a path from any starting position to a goal position

% No More Start Nodes to DFS
dfs_each_start_pos(_, _, []) :- fail.                           
dfs_each_start_pos(Board, Player, [Start|StartPositions]) :- 
    % write('# DFS '), write(Start), write(StartPositions), write(Player), nl, 
    dfs(Board, Player, [Start], []), !                          % Check if there's a path from Start to a Goal position
    ; %OR
    dfs_each_start_pos(Board, Player, StartPositions).          % Else: Check again starting from the next start position 

% No more Nodes to visit
dfs(_, _, [], _) :- fail.                         

% Skip Goal not found
dfs(Board, Player, [Node| Tail], Visited) :-
    %write('# TEST GOAL - NODE '), write(Node), nl,
    goal(Player, Node, Board).

% Skip Node already visited
dfs(Board, Player, [Node| Tail], Visited) :-
    %write('# VISITED  = '), write(Visited), nl,
    %write('# NODE = '), write(Node), nl,
    %write('# TAIL = '), write(Tail), nl,
    member(Node, Visited),
    dfs(Board, Player, Tail, Visited).  

% Check neighbors - mark visited and add unvisited neighbors to the end of the list
dfs(Board, Player, [Node| Tail], Visited) :-
    findall(Neighbor, (next_neighbor(Board, Player, Node, Neighbor), \+ member(Neighbor, Visited)), Neighbors),
    append(Tail, Neighbors, ToVisit),                           % Add unvisited neighbors to the end of the list
    append([Node], Visited, NewVisited),                        % Mark Node as visited
    %write('# TO VISIT = '), write(ToVisit), nl,
    %write('# NEW VISITED = '), write(NewVisited), nl,
    dfs(Board, Player, ToVisit, NewVisited).                    % Continue DFS with the new list of nodes to visit and the new list of visited nodes   

% Goal is Node having a specific coordinate (WHITE -> Last Column, BLACK -> Last Row)
goal(Player, [X, Y], Board) :-
    length(Board, Board_Length),
    (Player = 'WHITE' ->
        Y =:= Board_Length, !
        ;   %OR
        X =:= Board_Length,
    !).

% Get Next Neighbor of a Position with same color Piece
next_neighbor(Board, Player, [X, Y], [NewX, NewY]) :-
    neighbor_coords([X, Y], [NewX, NewY]),
    valid_coords(Board, [NewX, NewY]),
    piece_at(Board, Player, [NewX, NewY]).

neighbor_coords([X, Y], [NewX, NewY]) :-
    DXY = [[1, 0], [0, 1], [-1, 0], [0, -1], [1, -1], [-1, 1]],
    member([DX, DY], DXY),
    NewX is X + DX,
    NewY is Y + DY.

valid_coords(Board, [X, Y]) :-
    length(Board, Board_Length),
    X >= 1, X =< Board_Length,
    Y >= 1, Y =< Board_Length.

piece_at(Board, Player, [X, Y]) :-
    piece(Player, Piece),
    nth1(X, Board, Row),
    nth1(Y, Row, Piece).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               PLAYERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

% Get starting positions for a player (WHITE -> 1st Column, BLACK -> 1st Row)
start_positions(Board, Player, StartPositions) :-
    piece(Player, Piece),
    (Player = 'WHITE' ->
        findall([Y, 1], (nth1(Y, Board, Row), nth1(1, Row, Piece)), StartPositions)     
    ;   %OR  
        findall([1, X], (nth1(1, Board, Row), nth1(X, Row, Piece)), StartPositions)     
    ).

% Get players pieces chars
piece('BLACK', '\u2b21'). % ⬡
piece('WHITE', '\u2b22'). % ⬢

piece('EMPTY', '.'). % .

% Get next player
next_player('BLACK', 'WHITE').
next_player('WHITE', 'BLACK').