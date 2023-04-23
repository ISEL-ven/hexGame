:- module(minimax, [minimax/3]).

% minimax( Pos, BestSucc, Val):
%   Pos is a position, Val is its minimax value;
%   best move from Pos leads to position BestSucc

minimax( Pos, BestSucc, Val) :-
   moves( Pos, PosList), !,      % Legal moves in Pos produce PosList
   best( PosList, BestSucc, Val)
   ; % Or
   staticval( Pos, Val).         % Pos has no successors: evaluate statically
   
   
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


% implemented by ven - maybe not correct
moves( Pos, PosList) :-
   Pos == [], !
   ; % Or
   findall( simuated_play( Board, Player, NewBoard), 
            play( Board, Player, X, Y, NewBoard), 
            PosList).
   
% TODO -----------------------------------------------
staticval( Pos, Val) :-
   Pos == [], !, Val = 0
   ; % Or
   Pos == [], !, Val = 1
   ; % Or
   Pos == [], !, Val = -1.
   
% TODO -----------------------------------------------
min_to_move(Pos) :-
   nl.

% TODO -----------------------------------------------
max_to_move(Pos) :-
   nl.

simuated_play(Board, Player, NewBoard) :-
   member(X, Board_Length),
   member(Y, Board_Length),
   play(Board, Player, X, Y, NewBoard).

% TODO -----------------------------------------------
play(Board, Player, X, Y, NewBoard) :-
   nl.
