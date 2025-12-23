:- module(model, [
    ensure_string/2,
    ensure_key/2,
    instant_date/2,
    time_minutes/2,
    times_overlap/4,
    groups_overlap/2
]).

ensure_string(Value, String) :-
    ( string(Value) -> String = Value
    ; atom(Value)   -> atom_string(Value, String)
    ; number(Value) -> number_string(Value, String)
    ; term_string(Value, String)
    ).

ensure_key(Value, KeyAtom) :-
    ( atom(Value)   -> KeyAtom = Value
    ; string(Value) -> atom_string(KeyAtom, Value)
    ; number(Value) -> atom_number(KeyAtom, Value)
    ; term_string(Value, S), atom_string(KeyAtom, S)
    ).

instant_date(InstantAtom, DateString) :-
    ensure_string(InstantAtom, InstantStr),
    ( sub_string(InstantStr, 0, 10, _, DateString) -> true ; DateString = InstantStr ).

time_minutes(TimeAtom, Minutes) :-
    ensure_string(TimeAtom, TimeStr),
    split_string(TimeStr, ":", "", Parts),
    Parts = [HStr, MStr|_],
    number_string(H, HStr),
    number_string(M, MStr),
    Minutes is H * 60 + M.

times_overlap(Start1, End1, Start2, End2) :-
    Start1 < End2,
    Start2 < End1.

groups_overlap(G1, G2) :-
    member(X, G1),
    member(X, G2),
    !.
