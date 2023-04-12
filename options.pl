:- module(options, [print_options/0, read_options/0, print_dialog/1]).

:- use_module(game).

% ***************************************
%               OPTIONS
% ****************************************

print_options() :- 
    print_dialog('GAME MODE'),
    write('1 - Player vs Player\n'),
    write('2 - Player vs CPU\n'),
    write('3 - CPU vs CPU\n').
    
read_options() :- 
    write('Choose mode: '),
    read(Option),
    option(Option).

read_board_size(Size) :-                            
    print_dialog('Set board size (max: 9)'),
    write('Choose size: '),
    read(Size).

% Option 1 - Player vs Player
option(1) :- 
    read_board_size(Size),              % Get the size of the board from input
    start_game(1, Size).

% Option 2 - Player vs CPU
option(2) :-
    write('Option 2 TODO').

% Option 3 - CPU vs CPU
option(3) :- 
    write('Option 3 TODO').

option(_) :- 
    print_dialog('Choose a valid option!'),
    read_options().

print_dialog(Message) :-
    write('\n\u2b22 \u2b21 \u2b22  '),
    write(Message),
    write('  \u2b22 \u2b21 \u2b22\n').
