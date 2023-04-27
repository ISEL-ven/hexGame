:- use_module('../algo/alphabeta').
:- use_module('../game').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       Test - ALPHA-BETA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Board 2x2 - CPU always wins if it starts first
test_alphabeta_2x2_cpu_vs_cpu(Runtime) :- 
    write('\e[35mTesting 2X2 CPU vs CPU\e[0m'),

    %Act
    statistics(runtime,[Start|_]),
    start_game(2, 'WHITE_CPU', 'BLACK_CPU', 'alphabeta', Winner),
    statistics(runtime,[Stop|_]),
    Runtime is Stop - Start,
    
    % Assert
    Winner = 'WHITE_CPU', !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 3x3 - CPU always wins if it starts first 
test_alphabeta_3x3_cpu_vs_cpu(Runtime) :- 
    write('\e[35mTesting 3X3 CPU vs CPU\e[0m'),

    %Act
    statistics(runtime,[Start|_]),
    start_game(3, 'WHITE_CPU', 'BLACK_CPU', 'alphabeta', Winner),
    statistics(runtime,[Stop|_]),
    Runtime is Stop - Start,
    
    % Assert
    Winner = 'WHITE_CPU', !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 4x4 - CPU always wins if it starts first 
test_alphabeta_4x4_cpu_vs_cpu(Runtime) :- 
    write('\e[35mTesting 4X4 CPU vs CPU\e[0m'),

    %Act
    statistics(runtime,[Start|_]),
    start_game(4, 'WHITE_CPU', 'BLACK_CPU', 'alphabeta', Winner),
    statistics(runtime,[Stop|_]),
    Runtime is Stop - Start,
    
    % Assert
    Winner = 'WHITE_CPU', !
    ;   % OR
    write('TEST FAILED\n'), fail.

% RUN ALL TESTS
:-  
    write('\e[36mRunning ALPHABETA tests...\e[0m\n'),
    test_alphabeta_2x2_cpu_vs_cpu(Runtime2x2), 
    test_alphabeta_3x3_cpu_vs_cpu(Runtime3x3), 
    %test_alphabeta_4x4_cpu_vs_cpu(Runtime4x4), % Very slow,
    write('ALPHABETA\n'),
    write('Time to execute 2x2: '), write(Runtime2x2), write(' ms'), nl,
    write('Time to execute 3x3: '), write(Runtime3x3), write(' ms'), nl,
    %write('Time to execute 4x4: '), write(Runtime4x4), nl,
    write('\e[32mALL TESTS PASSED! :D\e[0m\n\n'), !
    ;   % OR
    write('\e[33m\nSOME TEST FAILED! :(\e[0m\n\n'), fail.
