:- use_module('../minimax').
:- use_module('../game').

% Board 2x2 - CPU always wins if it starts first
test_minimax_2x2_cpu_vs_cpu() :- 
    write('\e[35mTesting 2X2 CPU vs CPU\e[0m'),
    game:start_game(2, 'WHITE_CPU', 'BLACK_CPU', Winner),
    Winner = 'WHITE_CPU', !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 3x3 - CPU always wins if it starts first 
% TODO It PASS THE TEST but MINIMAX RUNS UNNECCESSARY TIMES...it could get the moves from the first calculation?
test_minimax_3x3_cpu_vs_cpu() :- 
    write('\e[35mTesting 3X3 CPU vs CPU\e[0m'),
    game:start_game(3, 'WHITE_CPU', 'BLACK_CPU', Winner),
    Winner = 'WHITE_CPU', !
    ;   % OR
    write('TEST FAILED\n'), fail.

% RUN ALL TESTS
:-  
    write('\e[36mmainRunning MINIMAX tests...It will take a long time...\e[0m\n'),
    test_minimax_2x2_cpu_vs_cpu(), 
    test_minimax_3x3_cpu_vs_cpu(),
    write('\e[32mALL TESTS PASSED! :D\e[0m\n\n'), !
    ;   % OR
    write('\e[33m\nSOME TEST FAILED! :(\e[0m\n\n'), fail.
