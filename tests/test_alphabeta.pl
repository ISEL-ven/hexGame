:- use_module('../algo/alphabeta').
:- use_module('../game').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       Test - ALPHA-BETA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Board Size x Size - First CPU always wins if it starts first
test_alphabeta_cpu_vs_cpu(Runtime, Size) :- 
    write('\e[35mTesting '),write(Size), write('X'), write(Size), write(' CPU vs CPU\e[0m'), 

    %Act
    statistics(runtime,[Start|_]),
    start_game(Size, 'WHITE_CPU', 'BLACK_CPU', 'alphabeta', Winner),
    statistics(runtime,[Stop|_]),
    Runtime is Stop - Start,
    
    % Assert
    Winner = 'WHITE_CPU', !
    ;   % OR
    write('TEST FAILED\n'), fail.

% RUN ALL TESTS
:-  
    write('\e[35mRunning ALPHABETA tests...\e[0m\n'),
    test_alphabeta_cpu_vs_cpu(Runtime2x2, 2), 
    test_alphabeta_cpu_vs_cpu(Runtime3x3, 3), 
    %test_alphabeta_cpu_vs_cpu(Runtime4x4, 4), % Very slow,
    write('ALPHABETA\n'),
    write('Time to execute 2x2: '), write(Runtime2x2), write(' ms'), nl,
    write('Time to execute 3x3: '), write(Runtime3x3), write(' ms'), nl,
    %write('Time to execute 4x4: '), write(Runtime4x4), nl,
    write('\e[32mALL TESTS PASSED! :D\e[0m\n\n'), !
    ;   % OR
    write('\e[33m\nSOME TEST FAILED! :(\e[0m\n\n'), fail.
