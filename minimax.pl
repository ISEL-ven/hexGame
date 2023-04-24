:- module(minimax, [minimax/3]).
:- use_module(game).


% minimax( Pos, BestSucc, Val):
%   Pos is a position, Val is its minimax value;
%   best move from Pos leads to position BestSucc

minimax( Pos, BestSucc, Val) :-
   write('\n# Minimax on = '),write(Pos),nl,
   moves( Pos, PosList), write('# PosList = '), write(PosList), nl, !,      % Legal moves in Pos produce PosList
   best( PosList, BestSucc, Val)
   ; % Or
   staticval( Pos, Val).         % Pos has no successors: evaluate statically
   
   
best( [Pos], Pos, Val) :-
   write('# best'), nl,
   minimax( Pos, _, Val), !.


best( [Pos1 | PosList], BestPos, BestVal) :-
   write('# best2'), nl,
   minimax( Pos1, _, Val1),
   best( PosList, Pos2, Val2),
   betterof( Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterof( Pos0, Val0, Pos1, Val1, Pos0, Val0) :- % Pos0 better than Pos1
   write('# betterof'), nl,
	min_to_move( Pos0),     % MIN to move in Pos0
	Val0 > Val1, !          % MAX prefers the greater value
	; % Or 
	max_to_move( Pos0),     % MAX to move in Pos0
	Val0 < Val1, !.         % MIN prefers the lesser value


betterof( Pos0, Val0, Pos1, Val1, Pos1, Val1). % Otherwise Pos1 better than Pos0

moves(Pos, PosList) :-
   get_next_player(Pos, Player), write('# TURN = '), write(Player), nl,
   findall(Pos1, simulated_play(Pos, Player, Pos1) , PosList).
 
simulated_play(Pos, Player, Pos1) :-
   game:empty_pos([X, Y], Pos), 
   write('SIMULATED PLAY on '), write(Pos), write('MOVE = '), write([X, Y]), nl,
   game:apply_move([X, Y], Player, Pos, Pos1),
   write('SIMULATED PLAY GENERATED '), write(NewBoard), nl.

staticval(Pos, Value) :-
   get_next_player(Pos, Player),
   staticval(Pos, Value, Player).

staticval(Pos, Value, 'WHITE') :-
   game:check_victory(Pos, 'WHITE'),
   Value is 1.

staticval(Pos, Value, 'BLACK') :-
   game:check_victory(Pos, 'BLACK'),
   Value is -1.

staticval(_, _, 0).

get_next_player(Board, Player) :-
   count_pieces(Board, '\u2b22', WhiteCount),
   count_pieces(Board, '\u2b21', BlackCount),
   write(WhiteCount), write(' '), write(BlackCount), nl,
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
   write(Piece), write(Row),
   findall(Piece, member(Piece, Row), Found),
   length(Found, Count).
   
% TODO -----------------------------------------------
min_to_move(Pos) :- write('min ').
  

% TODO -----------------------------------------------
max_to_move(Pos) :-write('min ').
 
