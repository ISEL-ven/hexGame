:- module(minimax, [minimax/3]).
:- use_module(game).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               MINIMAX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% minimax( Pos, BestSucc, Val):
%   Pos is a position, Val is its minimax value;
%   best move from Pos leads to position BestSucc

minimax( Pos, BestSucc, Val) :-
   write('\n# Minimax on = '),write(Pos) ,nl,
   moves( Pos, PosList),  % Legal moves in Pos produce PosList
   best( PosList, BestSucc, Val),
   write('# PosList = '), write(PosList), nl,
   write('&&&&&&&& BestSucc = '), write(BestSucc), nl, !
   ; % Or
   staticval( Pos, Val),
   write('# VAL = '), write(Val), nl. % Pos has no successors: evaluate statically
   
best( [Pos], Pos, Val) :-
   write('# best'), nl,
   minimax( Pos, _, Val), !.

best( [Pos1 | PosList], BestPos, BestVal) :-
   write('# best2'), nl,
   minimax( Pos1, _, Val1),
   best( PosList, Pos2, Val2),
   betterof( Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterof( Pos0, Val0, Pos1, Val1, Pos0, Val0) :- % Pos0 better than Pos1
   write('# betterof '), write(Pos0), write(' val0 = '), write(Val0), write(' val1 = '), write(Val1),nl,
	min_to_move( Pos0),     % MIN to move in Pos0
	Val0 > Val1, !          % MAX prefers the greater value
	; % Or 
	max_to_move( Pos0),     % MAX to move in Pos0
	Val0 < Val1, !.         % MIN prefers the lesser value

betterof( Pos0, Val0, Pos1, Val1, Pos1, Val1). % Otherwise Pos1 better than Pos0

staticval(Pos, Value) :-
   game:check_victory(Pos, 'BLACK'),
   Value is 1 , write('%%%%%%% SIM BLACK WINS %%%%%%%\n').

staticval(Pos, Value) :-
   game:check_victory(Pos, 'WHITE'),
   Value is -1 , write('%%%%%%% SIM WHITE WINS %%%%%%%\n').

staticval(_, 0).

max_to_move(Pos) :-
   get_next_player(Pos, Player),
   Player = 'BLACK',
   write('MAX CUTTING\n').

min_to_move(Pos) :- 
   get_next_player(Pos, Player),
   Player = 'WHITE',
   write('MIN CUTTING\n').

moves(Pos, PosList) :-
   bagof(Pos1, (legal_moves(Pos, Pos1)) , PosList).

legal_moves(Pos, Pos1) :-
   get_next_player(Pos, Player), 
   get_curr_player(Player,CurrPlayer),
   not(game:check_victory(Pos, CurrPlayer)),
   game:empty_pos([X, Y], Pos),
   write('# SIM '), write(Player),write(' to MOVE = '), write([X, Y]), write(' on Board '), write(Pos),
   game:apply_move([X, Y], Player, Pos, Pos1),
   write(' ---> '), write(Pos1),nl.
  
   
% Calculates the next player to move by counting the number of pieces on the board
get_next_player(Board, Player) :-
   count_pieces(Board, '\u2b22', WhiteCount),
   count_pieces(Board, '\u2b21', BlackCount),
   (WhiteCount > BlackCount ->
      Player = 'BLACK', !
      ; % Or
      Player = 'WHITE').

get_curr_player('WHITE', 'BLACK').
get_curr_player('BLACK', 'WHITE').

count_pieces([], _, 0).
count_pieces([Row|Rest], Char, Count) :-
    count_char_row(Row, Char, RowCount),
    count_pieces(Rest, Char, RestCount),
    Count is RowCount + RestCount.
count_char_row(Row, Piece, Count) :-
   findall(Piece, member(Piece, Row), Found),
   length(Found, Count).
