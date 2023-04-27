:- module(minimax, [minimax/3]).

:- use_module('../game').

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
   (check_victory(Pos, 'WHITE') ->
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
