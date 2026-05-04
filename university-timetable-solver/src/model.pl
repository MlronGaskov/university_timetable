:- module(model, [
    ensure_string/2,
    ensure_key/2,
    get_dict_default/4,
    dict_get_any/3,
    dict_get_any_default/4,
    instant_date/2,
    time_minutes/2,
    times_overlap/4,
    groups_overlap/2,
    slot_bit/2,
    pair_key/3
]).

ensure_string(Value, String) :-
    (   string(Value) -> String = Value
    ;   atom(Value)   -> atom_string(Value, String)
    ;   number(Value) -> number_string(Value, String)
    ;   term_string(Value, String)
    ).

ensure_key(Value, KeyAtom) :-
    (   atom(Value)   -> KeyAtom = Value
    ;   string(Value) -> atom_string(KeyAtom, Value)
    ;   number(Value) -> atom_number(KeyAtom, Value)
    ;   term_string(Value, S), atom_string(KeyAtom, S)
    ).

get_dict_default(Key, Dict, Default, Value) :-
    (   get_dict(Key, Dict, V)
    ->  Value = V
    ;   Value = Default
    ).

dict_get_any(Dict, [Key|_], Value) :-
    get_dict(Key, Dict, Value),
    !.
dict_get_any(Dict, [_|Rest], Value) :-
    dict_get_any(Dict, Rest, Value).

dict_get_any_default(Dict, Keys, Default, Value) :-
    (   dict_get_any(Dict, Keys, V)
    ->  Value = V
    ;   Value = Default
    ).

instant_date(InstantAtom, DateString) :-
    ensure_string(InstantAtom, InstantStr),
    (   sub_string(InstantStr, 0, 10, _, DateString)
    ->  true
    ;   DateString = InstantStr
    ).

time_minutes(TimeAtom, Minutes) :-
    ensure_string(TimeAtom, TimeStr),
    split_string(TimeStr, ":", "", Parts),
    Parts = [HStr, MStr|_],
    number_string(H, HStr),
    number_string(M, MStr),
    Minutes is H * 60 + M.

times_overlap(Start1, End1, Start2, End2) :-
    time_minutes(Start1, S1),
    time_minutes(End1, E1),
    time_minutes(Start2, S2),
    time_minutes(End2, E2),
    S1 < E2,
    S2 < E1.

groups_overlap(G1, G2) :-
    member(X, G1),
    member(Y, G2),
    ensure_string(X, XS),
    ensure_string(Y, YS),
    XS = YS,
    !.

slot_bit(SlotIdx, Bit) :-
    Bit is 1 << (SlotIdx - 1).

pair_key(A0, B0, Key) :-
    ensure_string(A0, A),
    ensure_string(B0, B),
    format(string(S), "~s|~s", [A, B]),
    ensure_key(S, Key).
