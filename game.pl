:- module(game, [start_game/5, check_victory/2, moves/2, get_next_player/2, piece/2]).

:- use_module(ui).
:- use_module(board).
:- use_module('./algo/minimax').
:- use_module('./algo/alphabeta').
:- use_module('./algo/dfs').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               GAME
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_game(Size, Player1, Player2, Algo, Winner) :-                          
    print_dialog('GAME STARTED'),
    create_board(Size, Board),                              % Create the Board based on Size given
    print_board(Board),                                     % Print initial Board  
    game_loop(Board, Player1, Player2, Algo, Winner).       % Start the Game loop

game_loop(Board, Player, NextPlayer, Algo, Winner) :-              
    ((Player = 'WHITE'; Player = 'BLACK') ->                % If Player -> Get Move from Player and apply it to Board
        write(Player), write(' to move. (row/column)\n'),                 
        get_move(Board, Move),                                 
        apply_move(Move, Player, Board, NewBoard), !        
        ;   %OR                                             % Else: Get Move from CPU
        write(Player), write(' is Thinking...\n'),
        get_move_cpu(Board, NewBoard, Algo) 
    ),

    print_board(NewBoard),                                  % Print the New Board 
    ((not(check_victory(NewBoard, Player))) ->              % If no Victory ->                  
        game_loop(NewBoard, NextPlayer, Player, Algo, Winner), !  % Restart loop with the new Board and switch Players
        ;   %OR
        Winner = Player                                     % Else: Game Over -> Return Winner
    ).                                   

%****************************************
%               MOVES
%****************************************

% Get CPU Move
% Minimax or AlphaBeta
get_move_cpu(Board, BestSucc, 'minimax') :-
    minimax(Board, BestSucc, _).

get_move_cpu(Board, BestSucc, 'alphabeta') :-
    alphabeta(Board, -inf, inf, BestSucc, _).

% Get Player Move
% Input and returns a Move if is valid. e.g. Input = 3/b -> Move = [3,b]
get_move(Board, Move) :-
    repeat,  
        read(Input),
        (
            convert_input(Input, Move),             % Convert input into a Move
            validate_move(Move, Board), !           % Validate if Move is valid to play
            ;   % OR
            write('Invalid move.\n'), fail          % Else: Re-read new input
        ).

% Apply Player Move to Board
apply_move(Move, Player, Board, NewBoard) :-
    piece(Player, Piece),
    modify_board(Move, Board, Piece, NewBoard).

convert_input(X/YLetter, Move) :-
    number(X),                                  % Check if X is a number
    nonvar(YLetter),                            % Check if YLetter is not capitalized/a variable
    atom_codes(YLetter, [YCode]),               % Get code of YLetter
    Y is YCode - 96,                            % Get Y as Number. e.g. a = 1
    Move = [X,Y].                         

% Check Move is valid
validate_move([X,Y], Board) :-
    length(Board, Board_Length),                % Get Board length
    X >= 1, X =< Board_Length,                  % Check X is on Board limits
    Y >= 1, Y =< Board_Length,                  % Check Y is on Board limits
    empty_pos([X,Y], Board).                    % Check if position is empty
    
% Check if Piece in position [X,Y] of the Board is empty (empty = '.')
empty_pos([X, Y], Board) :-
    nth1(X, Board, Row),
    nth1(Y, Row, Piece),
    (Piece == '.').

% Generate Boards with all legal Moves possible in the Board. Fails if no Moves
moves(Pos, PosList) :-
    bagof(Pos1, (legal_moves(Pos, Pos1)) , PosList).
    
% Get the legal Moves (represented as Board's (Pos1)) possible in Board "Pos"
legal_moves(Pos, Pos1) :-
    get_next_player(Pos, Player),                % Get Next player who's making the move in Board Pos
    get_opponent(Player,CurrPlayer),             % Get the Opponent
    not(check_victory(Pos, CurrPlayer)),         % Check Opponent didn't already win in Board Pos
    empty_pos([X, Y], Pos),                      % Get empty positions in Pos
    apply_move([X, Y], Player, Pos, Pos1).       % Apply the Move and return new Board Pos1
    %write('# SIM '), write(Player),write(' to MOVE = '), write([X, Y]), write(' on Board '), write(Pos), write(' ---> '), write(Pos1), nl.
    
% Calculates the next Player to Move by counting the number of Pieces on the Board
get_next_player(Board, Player) :-
    count_pieces(Board, '\u2b22', WhiteCount),
    count_pieces(Board, '\u2b21', BlackCount),
    (WhiteCount > BlackCount ->
        Player = 'BLACK', !
        ;   % Or
        Player = 'WHITE').

% Counts Elements in a Bidimensional List
count_pieces([], _, 0).
count_pieces([Row|Rest], Char, Count) :-
    count_char_row(Row, Char, RowCount),
    count_pieces(Rest, Char, RestCount),
    Count is RowCount + RestCount.
count_char_row(Row, Piece, Count) :-
    findall(Piece, member(Piece, Row), Found),
    length(Found, Count).
    
%****************************************
%           Check Victory - DFS
%****************************************

% Check if there is a path from any starting position to a goal position
% Start Positions (WHITE -> 1st Column, BLACK -> 1st Row)
check_victory(Board, Player) :-
    start_positions(Board, Player, StartPositions),             % Get start positions for Player
    dfs_each_start_pos(Board, Player, StartPositions).          % Dfs all start positions

% No Start Position to DFS
dfs_each_start_pos(_, _, []) :- fail.    
% Start DFS from each Start Position                       
dfs_each_start_pos(Board, Player, [Start|StartPositions]) :- 
    dfs(Board, Player, [Start], []), !                          % Check if there's a path from Start to a Goal position
    ;   %OR
    dfs_each_start_pos(Board, Player, StartPositions).          % Else: Check again starting from the next start position 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               PLAYERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

% Get starting positions for a Player (WHITE -> 1st Column, BLACK -> 1st Row)
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
% Get opponent
get_opponent('WHITE', 'BLACK').
get_opponent('BLACK', 'WHITE').
