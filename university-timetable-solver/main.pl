:- module(main, [main/1]).
:- use_module(server).

:- initialization(main, main).

main(Argv) :-
    catch(set_prolog_flag(stack_limit, 8_000_000_000), _, true),
    ( Argv = [PortAtom|_],
      atom_number(PortAtom, Port)
    -> true
    ;  Port = 5000
    ),
    start_server(Port),
    thread_get_message(_).
