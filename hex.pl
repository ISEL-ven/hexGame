:- use_module(ui).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               MAIN 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() :- 
    print_welcome(),
    menu().

:- main.  % run main automatically