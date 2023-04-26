:- module(minimax, [minimax/3]).
:- use_module(game).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               MINIMAX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% minimax( Pos, BestSucc, Val):
%   Pos is a position (Board), Val is its minimax value;
%   Best move from Board Pos leads to Board BestSucc

minimax( Pos, BestSucc, Val) :-
   moves( Pos, PosList),                     % Legal moves in Pos produce PosList
   best( PosList, BestSucc, Val), !
   %write('\n# Minimax on = '),write(Pos) ,nl,write('# PosList = '), write(PosList), nl, write('&&&&&&&& BestSucc = '), write(BestSucc), nl, !
   ; % Or
   staticval( Pos, Val).                     % Pos has no successors: evaluate statically
   %write('# VAL = '), write(Val), nl. 
   
best( [Pos], Pos, Val) :-
   minimax( Pos, _, Val), !.

best( [Pos1 | PosList], BestPos, BestVal) :-
   minimax( Pos1, _, Val1),
   best( PosList, Pos2, Val2),
   betterof( Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterof( Pos0, Val0, Pos1, Val1, Pos0, Val0) :- % Pos0 better than Pos1
	min_to_move( Pos0),     % MIN to move in Pos0
	Val0 > Val1, !          % MAX prefers the greater value
	; % Or 
	max_to_move( Pos0),     % MAX to move in Pos0
	Val0 < Val1, !.         % MIN prefers the lesser value

betterof( Pos0, Val0, Pos1, Val1, Pos1, Val1). % Otherwise Pos1 better than Pos0

staticval(Pos, Value) :-
   (game:check_victory(Pos, 'WHITE') ->
      Value is 1, !
      ;  %Or
      Value is -1
   ).

staticval(_, 0).

max_to_move(Pos) :-
   get_next_player(Pos, Player),
   Player = 'WHITE'.

min_to_move(Pos) :- 
   get_next_player(Pos, Player),
   Player = 'BLACK'.

moves(Pos, PosList) :-
   bagof(Pos1, (legal_moves(Pos, Pos1)) , PosList).


% Get the legal moves (represented as Board's (Pos1)) possible in Board "Pos"
legal_moves(Pos, Pos1) :-
   get_next_player(Pos, Player),                % Get Next player whos making the move in Board Pos
   get_opponent(Player,CurrPlayer),             % Get the Opponent
   not(game:check_victory(Pos, CurrPlayer)),    % Check Opponent didn't already win in Board Pos
   game:empty_pos([X, Y], Pos),                 % Get empty positions in Pos
   game:apply_move([X, Y], Player, Pos, Pos1).  % Apply the move and return new Board Pos1
   %write('# SIM '), write(Player),write(' to MOVE = '), write([X, Y]), write(' on Board '), write(Pos), write(' ---> '), write(Pos1), nl.
  
% Calculates the next player to move by counting the number of pieces on the board
get_next_player(Board, Player) :-
   count_pieces(Board, '\u2b22', WhiteCount),
   count_pieces(Board, '\u2b21', BlackCount),
   (WhiteCount > BlackCount ->
      Player = 'BLACK', !
      ; % Or
      Player = 'WHITE').

count_pieces([], _, 0).
count_pieces([Row|Rest], Char, Count) :-
    count_char_row(Row, Char, RowCount),
    count_pieces(Rest, Char, RestCount),
    Count is RowCount + RestCount.
count_char_row(Row, Piece, Count) :-
   findall(Piece, member(Piece, Row), Found),
   length(Found, Count).

get_opponent('WHITE', 'BLACK').
get_opponent('BLACK', 'WHITE').
