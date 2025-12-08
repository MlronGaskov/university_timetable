:- [solver].

:- initialization(main, main).

main(_) :-
    start_server(5000),
    thread_get_message(_).
