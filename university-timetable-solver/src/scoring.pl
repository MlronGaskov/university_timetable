:- module(scoring, [
    soft_penalty/3,
    score_from_penalty/2
]).

:- use_module(library(lists)).
:- use_module(model).

score_from_penalty(Penalty, Score) :-
    Score is 100.0 / (100.0 + Penalty).

soft_penalty(Assignments, Policy, Penalty) :-
    penalty_lunch_fast(Assignments, Policy, Plunch),
    penalty_limits_fast(Assignments, Policy, Plimits),
    Penalty is Plunch + Plimits.

pair_key(A0, B0, Key) :-
    ensure_string(A0, A),
    ensure_string(B0, B),
    format(string(S), "~s|~s", [A, B]),
    ensure_key(S, Key).

penalty_lunch_fast(Assignments, Policy, Pen) :-
    get_dict(breaks, Policy, Breaks),
    get_dict(lunch, Breaks, Lunch),
    ( get_dict(enabled, Lunch, true)
    -> get_dict(windowStart, Lunch, WS),
       get_dict(windowEnd, Lunch, WE),
       get_dict(minFreeMinutes, Lunch, MinFree),
       time_minutes(WS, WSM),
       time_minutes(WE, WEM),
       weight(Policy, missingLunch, W),
       lunch_penalty_for_teachers(Assignments, WSM, WEM, MinFree, W, PT),
       lunch_penalty_for_groups(Assignments, WSM, WEM, MinFree, W, PG),
       Pen is PT + PG
    ;  Pen = 0
    ).

lunch_penalty_for_teachers(Assignments, WSM, WEM, MinFree, W, Pen) :-
    build_intervals_dict(Assignments, teacher, Dict),
    lunch_penalty_from_dict(Dict, WSM, WEM, MinFree, W, Pen).

lunch_penalty_for_groups(Assignments, WSM, WEM, MinFree, W, Pen) :-
    build_intervals_dict(Assignments, group, Dict),
    lunch_penalty_from_dict(Dict, WSM, WEM, MinFree, W, Pen).

build_intervals_dict(Assignments, teacher, DictOut) :-
    foldl(add_interval_teacher, Assignments, _{}, DictOut).
build_intervals_dict(Assignments, group, DictOut) :-
    foldl(add_interval_groups, Assignments, _{}, DictOut).

add_interval_teacher(A, D0, D1) :-
    T = A.teacherId,
    Day = A.dayOfWeek,
    time_minutes(A.startTime, S),
    time_minutes(A.endTime, E),
    pair_key(T, Day, Key),
    add_interval(D0, Key, int(S,E), D1).

add_interval_groups(A, D0, D1) :-
    Day = A.dayOfWeek,
    time_minutes(A.startTime, S),
    time_minutes(A.endTime, E),
    foldl(add_one_group_interval(Day, int(S,E)), A.groups, D0, D1).

add_one_group_interval(Day, Int, G, D0, D1) :-
    pair_key(G, Day, Key),
    add_interval(D0, Key, Int, D1).

add_interval(D0, Key, Int, D1) :-
    ( get_dict(Key, D0, L0) -> L1 = [Int|L0] ; L1 = [Int] ),
    put_dict(Key, D0, L1, D1).

lunch_penalty_from_dict(Dict, WSM, WEM, MinFree, W, Pen) :-
    dict_pairs(Dict, _, Pairs),
    findall(P,
        (
            member(_Key-Ints0, Pairs),
            sort_intervals(Ints0, Ints),
            free_max_in_window(Ints, WSM, WEM, MaxFree),
            ( MaxFree < MinFree -> P is W ; P is 0 )
        ),
        Ps),
    sum_list(Ps, Pen).

sort_intervals(Ints, Sorted) :-
    map_list_to_pairs(int_start, Ints, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

int_start(int(S,_), S).

free_max_in_window(Ints, WSM, WEM, MaxFree) :-
    clip_intervals(Ints, WSM, WEM, Clipped),
    merge_intervals(Clipped, Merged),
    max_free_gap(Merged, WSM, WEM, MaxFree).

clip_intervals([], _, _, []).
clip_intervals([int(S,E)|Rest], WSM, WEM, Out) :-
    S1 is max(S, WSM),
    E1 is min(E, WEM),
    ( S1 < E1 -> Out = [int(S1,E1)|Tail] ; Out = Tail ),
    clip_intervals(Rest, WSM, WEM, Tail).

merge_intervals([], []).
merge_intervals([I|Rest], Merged) :-
    merge_intervals_acc(Rest, I, [], Rev),
    reverse(Rev, Merged).

merge_intervals_acc([], Curr, Acc, [Curr|Acc]).
merge_intervals_acc([int(S,E)|Rest], int(CS,CE), Acc, Out) :-
    ( S =< CE
    -> NE is max(CE, E),
       merge_intervals_acc(Rest, int(CS,NE), Acc, Out)
    ;  merge_intervals_acc(Rest, int(S,E), [int(CS,CE)|Acc], Out)
    ).

max_free_gap([], WSM, WEM, Max) :- Max is WEM - WSM.
max_free_gap([int(S,E)|Rest], WSM, WEM, Max) :-
    FirstGap is S - WSM,
    max_free_gap_rest(Rest, E, WEM, TailMax),
    Max is max(FirstGap, TailMax).

max_free_gap_rest([], LastEnd, WEM, Max) :- Max is WEM - LastEnd.
max_free_gap_rest([int(S,E)|Rest], LastEnd, WEM, Max) :-
    Gap is S - LastEnd,
    max_free_gap_rest(Rest, E, WEM, TailMax),
    Max is max(Gap, TailMax).

penalty_limits_fast(Assignments, Policy, Pen) :-
    limits_params(Policy, P),
    build_masks(Assignments, teacher, TDict),
    build_masks(Assignments, group, GDict),
    penalty_from_masks(TDict, P.maxTDay, P.maxTCons, P.slotsN, P.wDay, P.wCons, PT),
    penalty_from_masks(GDict, P.maxGDay, P.maxGCons, P.slotsN, P.wDay, P.wCons, PG),
    Pen is PT + PG.

limits_params(Policy, params{
    slotsN:SlotsN,
    maxTDay:MaxTDay, maxGDay:MaxGDay,
    maxTCons:MaxTCons, maxGCons:MaxGCons,
    wDay:WDay, wCons:WCons
}) :-
    get_dict(grid, Policy, Grid),
    get_dict(slots, Grid, Slots),
    length(Slots, SlotsN),

    get_dict(limits, Policy, Limits),
    get_dict(maxPairsPerDay, Limits, MPD),
    get_dict(maxConsecutivePairs, Limits, MCP),
    get_dict(teacher, MPD, MaxTDay),
    get_dict(group, MPD, MaxGDay),
    get_dict(teacher, MCP, MaxTCons),
    get_dict(group, MCP, MaxGCons),

    get_dict(weights, Policy, W),
    get_dict(soft, W, SoftW),
    get_dict(tooManyPairsPerDay, SoftW, WDay),
    get_dict(tooManyConsecutive, SoftW, WCons).

build_masks(Assignments, teacher, DictOut) :-
    foldl(add_mask_teacher, Assignments, _{}, DictOut).
build_masks(Assignments, group, DictOut) :-
    foldl(add_mask_groups, Assignments, _{}, DictOut).

bit_for_slot(SlotIdx, Bit) :-
    Bit is 1 << (SlotIdx - 1).

add_mask_teacher(A, D0, D1) :-
    bit_for_slot(A.slotIndex, Bit),
    pair_key(A.teacherId, A.dayOfWeek, Key),
    or_mask(D0, Key, Bit, D1).

add_mask_groups(A, D0, D1) :-
    bit_for_slot(A.slotIndex, Bit),
    Day = A.dayOfWeek,
    foldl(or_one_group(Day, Bit), A.groups, D0, D1).

or_one_group(Day, Bit, G, D0, D1) :-
    pair_key(G, Day, Key),
    or_mask(D0, Key, Bit, D1).

or_mask(D0, Key, Bit, D1) :-
    ( get_dict(Key, D0, M0) -> M1 is M0 \/ Bit ; M1 = Bit ),
    put_dict(Key, D0, M1, D1).

penalty_from_masks(Dict, MaxDay, MaxCons, SlotsN, WDay, WCons, Pen) :-
    dict_pairs(Dict, _, Pairs),
    findall(P,
        (
            member(_Key-Mask, Pairs),
            popcount(Mask, Count),
            EDay is max(0, Count - MaxDay),
            max_run(Mask, SlotsN, Run),
            ECons is max(0, Run - MaxCons),
            P is EDay * WDay + ECons * WCons
        ),
        Ps),
    sum_list(Ps, Pen).

popcount(0, 0) :- !.
popcount(N, C) :-
    N > 0,
    N1 is N /\ (N - 1),
    popcount(N1, C1),
    C is C1 + 1.

max_run(Mask, SlotsN, Max) :-
    max_run_(1, SlotsN, Mask, 0, 0, Max).

max_run_(I, N, _Mask, _Curr, Best, Best) :-
    I > N, !.
max_run_(I, N, Mask, Curr, Best, Max) :-
    Bit is 1 << (I - 1),
    ( Mask /\ Bit =\= 0 -> Curr1 is Curr + 1 ; Curr1 = 0 ),
    Best1 is max(Best, Curr1),
    I1 is I + 1,
    max_run_(I1, N, Mask, Curr1, Best1, Max).

weight(Policy, Key, W) :-
    get_dict(weights, Policy, Weights),
    get_dict(soft, Weights, Soft),
    get_dict(Key, Soft, W).
