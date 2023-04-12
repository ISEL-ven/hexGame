% Define the size of the board
board_size(11).

% Define the players
player(red).
player(blue).

% Define the starting position
start_position([]).

% Define the winning positions for each player
winning_position(red, Board) :-
    board_size(Size),
    path(Board, red, (0,_), (Size,_)).

winning_position(blue, Board) :-
    board_size(Size),
    path(Board, blue, (_,0), (_,Size)).

% Define a path on the board for a player from one point to another
path(Board, Player, From, To) :-
    move(Board, Player, From, Next),
    (
        Next = To;
        path(Board, Player, Next, To)
    ).

% Define a move on the board for a player from one point to another
move(Board, Player, (X,Y), (X1,Y1)) :-
    neighbour((X,Y), (X1,Y1)),
    \+ member((X1,Y1), Board),
    \+ opponent(Player,Oppo),
    \+ member((Oppo,(X1,Y1)), Board).

% Define neighbours on the board
neighbour((X,Y), (X1,Y)) :- X1 is X+1.
neighbour((X,Y), (X1,Y)) :- X1 is X-1.
neighbour((X,Y), (X,Y1)) :- Y1 is Y+1.
neighbour((X,Y), (X,Y1)) :- Y1 is Y-1.
neighbour((X,Y), (X1,Y)) :- X < Y,
                             X > 0,
                             X < Size,
                             Y > 0,
                             Y < Size,
                             Size is Size-2,
                             X2 is X+Size,
                             Y2 is Y+Size,
                             (
                                 neighbour((X2,Y2),(A,B));
                                 neighbour((A,B),(C,D))
                             ),
                             A >= 0,
                             B >= 0,
                             C >= 0,
                             D >= 0.

% Define opponent players
opponent(red ,blue).
opponent(blue ,red).


% Define the game loop
game_loop(Board, Player) :-
    % Print the current board
    print_board(Board),
    % Check if the current player has won
    (
        winning_position(Player, Board) ->
        format("~w wins!~n", [Player]);
        % If not, get the next move from the current player
        format("~w's move: ", [Player]),
        read(Move),
        % Make the move on the board
        make_move(Board, Move, NewBoard),
        % Switch to the other player and continue the game loop
        opponent(Player, Opponent),
        game_loop(NewBoard, Opponent)
    ).

% Define how to make a move on the board
make_move(Board, Move, NewBoard) :-
    \+ member(Move, Board),
    append(Board,[Move],NewBoard).

% Define how to print the board
print_board(Board) :-
    board_size(Size),
    between(1, Size, Row),
    (
        between(1, Size, Col),
        (
            member((red,(Row,Col)), Board) ->
            write('r');
            (
                member((blue,(Row,Col)), Board) ->
                write('b');
                write('.')
            )
        ),
        nl
    ),
    nl.

% Define how to validate a move
validate_move(Board, (Player,(Row,Col))) :-
    board_size(Size),
    Row > 0,
    Row =< Size,
    Col > 0,
    Col =< Size,
    \+ member((Player,(Row,Col)), Board),
    \+ opponent(Player,Oppo),
    \+ member((Oppo,(Row,Col)), Board).

% Define how to check for draw conditions
draw_condition(Board) :-
    board_size(Size),
    findall((X,Y), (between(1,Size,X), between(1,Size,Y)), AllPositions),
    subtract(AllPositions, Board, EmptyPositions),
    length(EmptyPositions, 0).

% Start a new game with an empty board and red as the first player
:- game_loop([], red).
