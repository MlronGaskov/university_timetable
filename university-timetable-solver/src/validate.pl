:- module(validate, [ensure_required/1]).

ensure_required(Body) :-
    must_have_dict(semester, Body),
    must_have_list(courses, Body),
    must_have_list(rooms, Body),
    must_have_list(teachers, Body),
    must_have_dict(policy, Body).

must_have_dict(Key, Dict) :-
    ( get_dict(Key, Dict, V), is_dict(V)
    -> true
    ;  format(string(Msg), "Missing or invalid field: ~w", [Key]),
       throw(error(invalid_request(Msg), _))
    ).

must_have_list(Key, Dict) :-
    ( get_dict(Key, Dict, V), is_list(V)
    -> true
    ;  format(string(Msg), "Missing or invalid field: ~w", [Key]),
       throw(error(invalid_request(Msg), _))
    ).
