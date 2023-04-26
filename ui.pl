:- module(ui, [menu/0, print_dialog/1, print_winner/1, print_welcome/0]).

:- use_module(game).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               UI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Main menu options
print_options() :- 
    print_dialog('Game Mode'),
    write('1 - Player vs Player\n'),
    write('2 - Player vs CPU\n'),
    write('3 - CPU vs Player\n').

% Main menu 
menu() :- 
    print_options(),
    write('Choose mode: '),
    read(Option),                               % Read option from user
    select_gamemode(Option, Player1, Player2),  % Select game mode 
    read_board_size(Size),                      % Read Board size from user
    start_game(Size, Player1, Player2).         % Start Game with selected options

% Option 1 - Player vs Player
select_gamemode(1, 'WHITE', 'BLACK').
% Option 2 - Player vs CPU
select_gamemode(2, 'WHITE', 'BLACK_CPU').
% Option 3 - CPU vs Player
select_gamemode(3, 'WHITE_CPU', 'BLACK').
% Option Invalid
select_gamemode(_,_,_) :- 
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
    write(' \u2b22 \u2b21 '), write(Player), write(' WINS!\u2b22 \u2b21 \n'),
    write('\u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22\n\n').

print_welcome() :-
    write('\n\u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \n'),
    write(' \u2b22 \u2b21 \u2b22 HEX \u2b21 \u2b22 \u2b21 \n'),
    write('\u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \n').
