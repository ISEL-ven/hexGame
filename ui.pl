:- module(ui, [menu/0, print_dialog/1, print_winner/1, print_welcome/0]).

:- use_module(game).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               UI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Main menu 
menu() :- 
    print_gamemode_options(),                           % Print Game Mode options
    read(OptionMode),                                   % Read option from user
    select_gamemode(OptionMode, Player1, Player2),      % Select Game Mode
    print_algo_options(),                               % Print Algorithm options
    read(OptionAlgo),                                   % Read option from user
    select_algo(OptionAlgo, Algo),                            % Select Algorithm mode 
    read_board_size(Size),                              % Read Board size from user
    start_game(Size, Player1, Player2, Algo, Winner),   % Start Game with selected options
    print_winner(Winner).                               % Game Over -> Print Winner
    
%
% Game Mode options
%
print_gamemode_options() :- 
    print_dialog('Game Mode'),
    write('1 - Player vs Player\n'),
    write('2 - CPU vs Player\n'),
    write('3 - Player vs CPU\n'),
    write('4 - CPU vs CPU\n'),
    write('Choose Game Mode: ').

select_gamemode(1, 'WHITE', 'BLACK').
select_gamemode(2, 'WHITE_CPU', 'BLACK').
select_gamemode(3, 'WHITE', 'BLACK_CPU').
select_gamemode(4, 'WHITE_CPU', 'BLACK_CPU').
select_gamemode(_,_,_) :- 
    print_dialog('Choose a valid option!'),
    menu().

%
% Algorithms options
%
print_algo_options() :- 
    print_dialog('CPU level'),
    write('1 - Minimax (Slower)\n'),
    write('2 - Alpha-Beta (Faster)\n'),
    write('Choose CPU level: ').

select_algo(1, 'minimax').
select_algo(2, 'alphabeta').
select_algo(_,_) :- 
    print_dialog('Choose a valid option!'),
    menu().

% Read board size from user
read_board_size(Size) :-                            
    print_dialog('Set board size'),
    write('Choose size: '),
    read(Size).

% Custom UI Messages
print_dialog(Message) :-
    write('\n\u2b22 \u2b21 \u2b22  '),
    write(Message),
    write('  \u2b22 \u2b21 \u2b22\n').

print_winner(Player) :-
    write('\n\u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \n'),
    write(' \u2b22 \u2b21 '), write(Player), write(' WINS! \u2b22 \u2b21 \n'),
    write('\u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22\n\n').

print_welcome() :-
    write('\n\u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22\n'),
    write(' \u2b22 \u2b21 \u2b22 HEX \u2b21 \u2b22 \u2b21 \n'),
    write('\u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21\n').
