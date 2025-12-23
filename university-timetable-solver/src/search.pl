:- module(search, [search_best/5]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- use_module(scoring).

search_best(TasksWithCands, Policy, BestAssignments, BestUnplaced, BestHardPenalty) :-
    cfg(Policy, Cfg),
    get_time(T0),
    run_restarts(TasksWithCands, Policy, Cfg, T0,
                 none, 1.0Inf,
                 BestAssignments, BestUnplaced, BestHardPenalty, _).

cfg(Policy, cfg{timeLimitMs:TL, restarts:MaxR, topK:TopK}) :-
    ( get_dict(search, Policy, S) -> true ; S = _{} ),
    ( get_dict(timeLimitMs, S, TL0) -> TL = TL0 ; TL = 1500 ),
    ( get_dict(restarts, S, R0) -> MaxR = R0 ; MaxR = 1000000 ),
    ( get_dict(topK, S, K0) -> TopK = K0 ; TopK = 3 ).

time_up(T0, TLms) :-
    get_time(T),
    ElapsedMs is (T - T0) * 1000.0,
    ElapsedMs >= TLms.

run_restarts(_Tasks, _Policy, Cfg, T0,
             Best0, BestTotal0,
             BestA, BestU, BestHard, BestTotal) :-
    time_up(T0, Cfg.timeLimitMs),
    !,
    finalize_best(Best0, BestTotal0, BestA, BestU, BestHard, BestTotal).
run_restarts(Tasks, Policy, Cfg, T0,
             Best0, BestTotal0,
             BestA, BestU, BestHard, BestTotal) :-
    ( Cfg.restarts =< 0
    -> finalize_best(Best0, BestTotal0, BestA, BestU, BestHard, BestTotal)
    ;  run_once(Tasks, Policy, Cfg.topK, A1, U1, Hard1, Total1),
       ( Total1 < BestTotal0
       -> Best1 = best(A1, U1, Hard1), BestTotal1 = Total1
       ;  Best1 = Best0, BestTotal1 = BestTotal0
       ),
       Cfg2 = Cfg.put(restarts, Cfg.restarts - 1),
       run_restarts(Tasks, Policy, Cfg2, T0,
                    Best1, BestTotal1,
                    BestA, BestU, BestHard, BestTotal)
    ).

finalize_best(none, _Inf, [], [], 1.0Inf, 1.0Inf) :- !.
finalize_best(best(A,U,Hard), Total, A, U, Hard, Total).

run_once(TasksWithCands, Policy, TopK, Assignments, Unplaced, HardPenalty, TotalPenalty) :-
    limits_params(Policy, Params),
    empty_occ(Occ0),
    greedy_build(TasksWithCands, TopK, Params,
                 Occ0, [], [], 0.0,
                 AssignsRev, UnplacedRev, HardPenalty),
    reverse(AssignsRev, Assignments),
    reverse(UnplacedRev, Unplaced),
    soft_penalty(Assignments, Policy, Soft),
    TotalPenalty is HardPenalty + Soft.

empty_occ(occ(R,T,G)) :-
    empty_assoc(R),
    empty_assoc(T),
    empty_assoc(G).

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

greedy_build([], _TopK, _Params, _Occ, A, U, P, A, U, P).
greedy_build([tc(Task, Cands)|Rest], TopK, Params,
             Occ0, A0, U0, P0,
             A, U, P) :-
    ( pick_best_candidate(Cands, Task, Occ0, Params, TopK, Cand)
    -> true
    ;  Cand = none
    ),
    apply_pick(Cand, Cands, Task, Occ0, A0, U0, P0, Occ1, A1, U1, P1),
    greedy_build(Rest, TopK, Params, Occ1, A1, U1, P1, A, U, P).

pick_best_candidate(Cands, Task, Occ, Params, TopK, Picked) :-
    findall(D-C,
        (
            member(C, Cands),
            C = cand(_,_,_,_,_),
            candidate_ok(C, Task, Occ),
            delta_limits(C, Task, Occ, Params, D)
        ),
        Pairs0),
    Pairs0 \= [],
    keysort(Pairs0, Sorted),
    take_k(Sorted, TopK, Top),
    random_member(_-Picked, Top).

take_k(List, K, Prefix) :-
    ( K =< 0 -> Prefix = []
    ; length(Prefix, K),
      append(Prefix, _, List)
    -> true
    ; Prefix = List
    ).

bit_for_slot(SlotIdx, Bit) :-
    Bit is 1 << (SlotIdx - 1).

get_mask(Assoc, Key, Mask) :-
    ( get_assoc(Key, Assoc, Mask0) -> Mask = Mask0 ; Mask = 0 ).

free_key(Assoc, Key, Bit) :-
    get_mask(Assoc, Key, Mask),
    Mask /\ Bit =:= 0.

candidate_ok(cand(RoomCode, Day, _Start, _End, SlotIdx),
             task(_CId, TeacherId, Groups, _ReqCap, _ReqItems, _Inst),
             occ(RoomOcc, TeacherOcc, GroupOcc)) :-
    bit_for_slot(SlotIdx, Bit),
    free_key(RoomOcc, room(Day, RoomCode), Bit),
    free_key(TeacherOcc, teacher(Day, TeacherId), Bit),
    groups_free(Groups, Day, Bit, GroupOcc).

groups_free([], _Day, _Bit, _GroupOcc).
groups_free([G|Gs], Day, Bit, GroupOcc) :-
    free_key(GroupOcc, group(Day, G), Bit),
    groups_free(Gs, Day, Bit, GroupOcc).

put_bit(Assoc0, Key, Bit, Assoc) :-
    get_mask(Assoc0, Key, Mask0),
    Mask is Mask0 \/ Bit,
    put_assoc(Key, Assoc0, Mask, Assoc).

apply_occ(cand(RoomCode, Day, _Start, _End, SlotIdx),
          task(_CId, TeacherId, Groups, _ReqCap, _ReqItems, _Inst),
          occ(RoomOcc0, TeacherOcc0, GroupOcc0),
          occ(RoomOcc1, TeacherOcc1, GroupOcc1)) :-
    bit_for_slot(SlotIdx, Bit),
    put_bit(RoomOcc0, room(Day, RoomCode), Bit, RoomOcc1),
    put_bit(TeacherOcc0, teacher(Day, TeacherId), Bit, TeacherOcc1),
    put_groups(Groups, Day, Bit, GroupOcc0, GroupOcc1).

put_groups([], _Day, _Bit, G0, G0).
put_groups([G|Gs], Day, Bit, G0, G2) :-
    put_bit(G0, group(Day, G), Bit, G1),
    put_groups(Gs, Day, Bit, G1, G2).

delta_limits(cand(_RoomCode, Day, _Start, _End, SlotIdx),
             task(_CId, TeacherId, Groups, _ReqCap, _ReqItems, _Inst),
             occ(_RoomOcc, TeacherOcc, GroupOcc),
             Params, Delta) :-
    bit_for_slot(SlotIdx, Bit),
    get_mask(TeacherOcc, teacher(Day, TeacherId), TMask0),
    TMask1 is TMask0 \/ Bit,
    delta_from_masks(TMask0, TMask1, Params.maxTDay, Params.maxTCons, Params.slotsN, Params.wDay, Params.wCons, DT),
    delta_groups(Groups, Day, Bit, GroupOcc, Params, DG),
    Delta is DT + DG.

delta_groups([], _Day, _Bit, _GroupOcc, _Params, 0.0).
delta_groups([G|Gs], Day, Bit, GroupOcc, Params, Delta) :-
    get_mask(GroupOcc, group(Day, G), M0),
    M1 is M0 \/ Bit,
    delta_from_masks(M0, M1, Params.maxGDay, Params.maxGCons, Params.slotsN, Params.wDay, Params.wCons, D1),
    delta_groups(Gs, Day, Bit, GroupOcc, Params, DRest),
    Delta is D1 + DRest.

delta_from_masks(M0, M1, MaxDay, MaxCons, SlotsN, WDay, WCons, Delta) :-
    popcount(M0, C0),
    popcount(M1, C1),
    E0 is max(0, C0 - MaxDay),
    E1 is max(0, C1 - MaxDay),
    DDay is (E1 - E0) * WDay,
    max_run(M0, SlotsN, R0),
    max_run(M1, SlotsN, R1),
    X0 is max(0, R0 - MaxCons),
    X1 is max(0, R1 - MaxCons),
    DCons is (X1 - X0) * WCons,
    Delta is DDay + DCons.

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

apply_pick(none, Cands, _Task, Occ, A0, U0, P0, Occ, A0, U1, P1) :-
    member(unplaced(UnplacedPenalty, CourseId, Reason, _TeacherId, _Groups), Cands),
    U1 = [unplaced_item(CourseId, Reason)|U0],
    P1 is P0 + UnplacedPenalty.

apply_pick(cand(RoomCode, Day, Start, End, SlotIdx), _Cands,
           task(CId, TeacherId, Groups, _ReqCap, _ReqItems, _Inst),
           Occ0, A0, U0, P0,
           Occ1, [A|A0], U0, P0) :-
    apply_occ(cand(RoomCode, Day, Start, End, SlotIdx),
              task(CId, TeacherId, Groups, 0, [], 0),
              Occ0, Occ1),
    A = _{
        courseId: CId,
        teacherId: TeacherId,
        groups: Groups,
        roomCode: RoomCode,
        dayOfWeek: Day,
        startTime: Start,
        endTime: End,
        slotIndex: SlotIdx
    }.
