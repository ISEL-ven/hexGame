:- module(options, [print_options/0, read_options/0, print_dialog/1]).

:- use_module(game).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               OPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_options() :- 
    print_dialog('GAME MODE'),
    write('1 - Player vs Player\n'),
    write('2 - Player vs CPU\n'),
    write('3 - CPU vs Player\n').
    
read_options() :- 
    write('Choose mode: '),
    read(Option),
    option(2).

read_board_size(Size) :-                            
    print_dialog('Set board size (max: 9)'),
    write('Choose size: '),
    read(Size).

% Option 1 - Player vs Player
option(1) :- 
    read_board_size(Size),
    start_game(1, Size).

% Option 2 - Player vs CPU
option(2) :-
    read_board_size(Size),
    start_game(2, Size).

% Option 3 - CPU vs Player
option(3) :- 
    read_board_size(Size),
    start_game(3, Size).

option(_) :- 
    print_dialog('Choose a valid option!'),
    read_options().

print_dialog(Message) :-
    write('\n\u2b22 \u2b21 \u2b22  '),
    write(Message),
    write('  \u2b22 \u2b21 \u2b22\n').

