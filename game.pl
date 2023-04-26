:- module(game, [start_game/3]).

:- use_module(ui).
:- use_module(board).
:- use_module(minimax).

%TODO BUG OPTION 3...SWAP MIN MAX WHEN GAME MODE OPTION 3?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               GAME
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_game(Size, Player1, Player2) :-                          
    print_dialog('GAME STARTED'),
    write('To play a piece write (e.g "1/a")\n'),
    create_board(Size, Board),                              % Create the Board based on Size given
    print_board(Board),                                     % Print initial Board  
    game_loop(Board, Player1, Player2).                     % Start the Game loop

 game_loop(Board, Player, NextPlayer) :-              
    ((Player = 'WHITE'; Player = 'BLACK') ->                % If Player -> Get Move from Player and apply it to Board
        write(Player), write(' to move\n'),                 
        get_move(Board, Move),                                 
        apply_move(Move, Player, Board, NewBoard), !        
        ;   %OR                                             % Else: Get Move from CPU
        write('CPU is Thinking...'),
        get_move_cpu(Board, NewBoard) 
    ),
    print_board(NewBoard),                                  % Print the New Board  
    not(check_victory(NewBoard, Player)),                   % If no Victory ->                  
    game_loop(NewBoard, NextPlayer, Player), !              % Restart loop with the new Board and switch Players
    ;   %OR
    print_winner(Player).                                   % Else: Game Over

%****************************************
%               MOVES
%****************************************

% Reads Input and returns a Move if is valid. e.g. Input = 3/b -> Move = [3,b]
get_move(Board, Move) :-
    repeat,  
        read(Input),
        (
            convert_input(Input, Move),                      % Convert input into a Move
            validate_move(Move, Board), !                    % Validate if Move is valid to play
            ;   % OR
            write('Invalid move.\n'), fail                   % Else: Re-read new input
        ).

get_move_cpu(Board, BestSucc) :-
    minimax(Board, BestSucc, _).

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
    dfs(Board, Player, [Start], []), !                          % Check if there's a path from Start to a Goal position
    ; %OR
    dfs_each_start_pos(Board, Player, StartPositions).          % Else: Check again starting from the next start position 

% No more Nodes to visit
dfs(_, _, [], _) :- fail.                         

% Skip - Goal not found
dfs(Board, Player, [Node| Tail], Visited) :-
    goal(Player, Node, Board).

% Skip - Node already visited
dfs(Board, Player, [Node| Tail], Visited) :-
    member(Node, Visited),
    dfs(Board, Player, Tail, Visited).  

% Check neighbors - mark visited and add unvisited neighbors to the end of the list
dfs(Board, Player, [Node| Tail], Visited) :-
    findall(Neighbor, (next_neighbor(Board, Player, Node, Neighbor),not(member(Neighbor, Visited))), Neighbors),
    append(Tail, Neighbors, ToVisit),                           % Add unvisited neighbors to the end of the list
    append([Node], Visited, NewVisited),                        % Mark Node as visited
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
    (Piece = '\u2b22' ->
        findall([Y, 1], (nth1(Y, Board, Row), nth1(1, Row, Piece)), StartPositions)     
    ;   %OR  
        findall([1, X], (nth1(1, Board, Row), nth1(X, Row, Piece)), StartPositions)     
    ).

% Get players pieces chars
piece('BLACK', '\u2b21').       % ⬡
piece('WHITE', '\u2b22').       % ⬢
piece('BLACK_CPU', '\u2b21').   % ⬡
piece('WHITE_CPU', '\u2b22').   % ⬢
piece('EMPTY', '.').            % .
