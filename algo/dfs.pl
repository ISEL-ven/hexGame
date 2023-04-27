:- module(dfs, [dfs/4]).

:- use_module('../game').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          DEPTH FIRST SEARCH           
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    piece(Player, Piece),
    (Piece = '\u2b22' ->
        Y =:= Board_Length, !
        ;   %OR
        X =:= Board_Length
    ).

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
