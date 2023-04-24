:- use_module(options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               MAIN 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() :- 
    write('\n\u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \n'),
    write(' \u2b22 \u2b21 \u2b22 HEX \u2b21 \u2b22 \u2b21 \n'),
    write('\u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \u2b21 \u2b22 \n'),
    print_options(),
    read_options().
:- main.  % run main automatically