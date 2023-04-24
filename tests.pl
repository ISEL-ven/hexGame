:- use_module(game).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       Test Check Victory
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Board 2x2 - WHITE WINS
%   A B
% 1 ⬡ . 1
%  2 ⬢ ⬢ 2
%     A B
%
test_2x2_white() :- 
    Board = [['\u2b21',.],['\u2b22','\u2b22']],
    Player = 'WHITE',
    board:print_board(Board),
    game:check_victory(Board, Player), ! 
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 2x2 - BLACK WINS
%   A B
% 1 ⬡ . 1
%  2 ⬡ ⬢ 2
%     A B
%
test_2x2_black() :- 
    Board = [['\u2b21',.],['\u2b21','\u2b22']],
    Player = 'BLACK',
    board:print_board(Board),
    game:check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 2x2 - NO ONE WINS
%   A B
% 1 ⬡ . 1
%  2 . ⬢ 2
%     A B
%
test_2x2_nowin() :- 
    Board = [['\u2b21',.],[.,'\u2b22']],
    Player = 'BLACK',
    board:print_board(Board),
    \+ game:check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 3x3 - BLACK WINS
%   A B C
% 1 ⬡ ⬢ ⬢ 1
%  2 ⬡ ⬡ ⬡ 2
%   3 ⬢ ⬢ ⬡ 3
%      A B C
%
test_3x3_black() :- 
    Board = [['\u2b21','\u2b22','\u2b22'],['\u2b21','\u2b21','\u2b21'],['\u2b22','\u2b22','\u2b21']],
    Player = 'BLACK',
    board:print_board(Board),
    game:check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 3x3 - WHITE WINS
%   A B C
% 1 ⬢ ⬢ ⬡ 1
%  2 ⬡ ⬢ ⬢ 2
%   3 ⬡ ⬡ ⬡ 3
%      A B C
%
test_3x3_white() :- 
    Board = [['\u2b22','\u2b22','\u2b21'],['\u2b21','\u2b22','\u2b22'],['\u2b21','\u2b21','\u2b21']],
    Player = 'WHITE',
    board:print_board(Board),
    game:check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.


% Board 3x3 - WHITE WINS
%   A B C
% 1 . ⬡ ⬢ 1
%  2 ⬡ ⬢ . 2
%   3 ⬢ ⬡ . 3
%      A B C
%
test_3x3_white1() :- 
    Board = [[.,'\u2b21','\u2b22'],['\u2b21','\u2b22',.],['\u2b22','\u2b21',.]],
    Player = 'WHITE',
    board:print_board(Board),
    game:check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 3x3 - BLACK WINS
%   A B C
% 1 . ⬢ ⬡ 1
%  2 ⬢ ⬡ . 2
%   3 ⬡ ⬢ . 3
%      A B C
%
test_3x3_black1() :- 
    Board = [[.,'\u2b22','\u2b21'],['\u2b22','\u2b21',.],['\u2b21','\u2b22',.]],
    Player = 'BLACK',
    board:print_board(Board),
    game:check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 5x5 - BLACK WINS
%   A B C D E
% 1 ⬡ ⬡ ⬡ ⬡ ⬡ 1
%  2 ⬢ ⬢ . ⬢ ⬡ 2
%   3 ⬡ ⬡ ⬡ . ⬡ 3
%    4 ⬡ ⬢ ⬡ ⬡ ⬡ 4
%     5 ⬡ ⬢ ⬢ . . 5
%        A B C D E
%
test_5x5_black() :- 
    Board = [
        ['\u2b21','\u2b21','\u2b21', '\u2b21', '\u2b21'],
        ['\u2b22','\u2b22',., '\u2b22', '\u2b21'],
        ['\u2b21','\u2b21','\u2b21', ., '\u2b21'],
        ['\u2b21','\u2b22','\u2b21', '\u2b21', '\u2b21'],
        ['\u2b21','\u2b22','\u2b22', ., .]],
    Player = 'BLACK',
    board:print_board(Board),
    game:check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 5x5 - WHITE WINS
% A B C D E
% 1 ⬢ ⬢ ⬢ ⬢ ⬡ 1
%  2 ⬢ ⬡ . ⬢ ⬡ 2
%   3 ⬢ ⬡ ⬢ . ⬢ 3
%    4 ⬢ ⬡ ⬢ ⬢ ⬡ 4
%     5 ⬢ ⬡ ⬢ . . 5
%        A B C D E
%
test_5x5_white() :- 
    Board = [
        ['\u2b22','\u2b22','\u2b22', '\u2b22', '\u2b21'],
        ['\u2b22','\u2b21',., '\u2b22', '\u2b21'],
        ['\u2b22','\u2b21','\u2b22', ., '\u2b22'],
        ['\u2b22','\u2b21','\u2b22', '\u2b22', '\u2b21'],
        ['\u2b22','\u2b21','\u2b22', ., .]],
    Player = 'WHITE',
    board:print_board(Board),
    game:check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.

% Board 5x5 - NO ONE WINS
%   A B C D E
% 1 ⬢ ⬢ . ⬢ ⬡ 1
%  2 ⬢ ⬡ . ⬢ ⬡ 2
%   3 . . . . .   3
%    4 ⬢ ⬡ . ⬢ ⬡ 4
%     5 ⬢ ⬡ . ⬢ ⬡ 5
%        A B C D E
%
test_5x5_nowin() :- 
    Board = [
        ['\u2b22','\u2b22',., '\u2b22', '\u2b21'],
        ['\u2b22','\u2b21',., '\u2b22', '\u2b21'],
        [.,.,., ., .],
        ['\u2b22','\u2b21',., '\u2b22', '\u2b21'],
        ['\u2b22','\u2b21',., '\u2b22','\u2b21']],
    Player = 'WHITE',
    board:print_board(Board),
    \+ game:check_victory(Board, Player), !
    ;   % OR
    write('TEST FAILED\n'), fail.


% RUN ALL TESTS
:-  test_2x2_white(), test_2x2_black(), test_2x2_nowin(), 
    test_3x3_black(), test_3x3_white(), test_3x3_white1(), test_3x3_black1(), 
    test_5x5_black(), test_5x5_white(), test_5x5_nowin(),
    write('\n:D :D :D ALL TESTS PASSED! :D :D :D\n\n'), !
    ;   % OR
    write('\nSOME TEST FAILED! :(\n'), fail.