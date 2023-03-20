% Board representation -----------------------------------------------------------------------------
board_size(11).
board(B) :- 
    board_size(N), 
    length(B, N), 
    maplist(length_list(N), B).

length_list(N, L) :- 
    length(L, N).

% Player representation ----------------------------------------------------------------------------
player(1, x).
player(2, o).

% Starting state -----------------------------------------------------------------------------------
start_state(state(B, 1)) :- 
    board(B).

% Move representation ------------------------------------------------------------------------------
move(Player, [X, Y]) :- 
    player(_, Player), 
    board_size(N), X >= 0, X < N, Y >= 0, Y < N.

% Apply a move to a board and return the new state -------------------------------------------------
apply_move(state(Board, Player), Move, state(NewBoard, NewPlayer)) :-
    move(Player, Move),
    get_cell(Board, Move, empty),
    set_cell(Board, Move, Player, NewBoard),
    switch_player(Player, NewPlayer).

% Game loop ----------------------------------------------------------------------------------------
play(State) :-
    (
        won(Player, State), 
        format('~w has won!', 
        [Player]), 
        nl
    )
    ;
    (
        draw(State),
        format('The game is a draw.'),
        nl
    )
    ;
    (
        read_move(Move), 
        apply_move(State, Move, NewState), 
        print_board(NewState), 
        play(NewState)
    ).

% Switch the current player ------------------------------------------------------------------------
switch_player(Player, NewPlayer) :- 
    player(N, _) = Player, 
    OtherN is 3 - N, 
    player(OtherN, NewPlayer).

% Check if the game is a draw ----------------------------------------------------------------------
draw(state(Board, _)) :- 
    \+ member(empty, Board).

% Check if a player has won ------------------------------------------------------------------------
won(Player, State) :-
    state(Board, _) = State,
    player(_, PlayerSymbol) = Player,
    (
        player1_wins(Board, PlayerSymbol); 
        player2_wins(Board, PlayerSymbol)
    ).

% Player 1 wins if there is a path from left to right ----------------------------------------------
player1_wins(Board, x) :- 
    path(Board, [0,0], [0,10], [], Visited), 
    member([0,10], Visited).

% Player 2 wins if there is a path from top to bottom ----------------------------------------------
player2_wins(Board, o) :- 
    path(Board, [0,0], [10,0], [], Visited), 
    member([10,0], Visited).

% Find a path on the board between two points ------------------------------------------------------
path(Board, Start, End, Visited, Path) :-
    append(_, [End], Path),
    path(Board, Start, End, Visited, Path, []).

path(_, End, End, _, Path, Path).

path(Board, Start, End, Visited, Path, Acc) :-
    adjacent(Start, Next),
    \+ member(Next, Visited),
    get_cell(Board, Next, Cell),
    Cell = empty,
    path(Board, Next, End, [Next|Visited], Path, [Next|Acc]).

% Get adjacent cells -------------------------------------------------------------------------------
adjacent([X, Y], [X1, Y1]) :- X1 is X+1, Y1 is Y.
adjacent([X, Y], [X1, Y1]) :- X1 is X, Y1 is Y+1.
adjacent([X, Y], [X1, Y1]) :- X1 is X-1, Y1 is Y+1.
adjacent([X, Y], [X1, Y1]) :- X1 is X-1, Y1 is Y.
adjacent([X, Y], [X1, Y1]) :- X1 is X, Y1 is Y-1.
adjacent([X, Y], [X1, Y1]) :- X1 is X+1, Y1 is Y-1.

% Get the value of a cell in the board =============================================================
get_cell(Board, [X, Y], Cell) :- 
    nth0(Y, Board, Row), 
    nth0(X, Row, Cell).

% Set the content of a cell ------------------------------------------------------------------------
set_cell(Board, [X, Y], Cell, NewBoard) :-
    nth0(Y, Board, Row), 
    nth0(X, Row, _),
    nth0(Y, NewBoard, NewRow), 
    nth0(X, NewRow, Cell),
    maplist(copy_term, [Row, NewRow], [RowCopy, NewRowCopy]),
    RowCopy = NewRowCopy.

