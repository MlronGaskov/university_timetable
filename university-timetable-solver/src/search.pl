:- module(search, [search_best/8]).

:- use_module(library(lists)).
:- use_module(library(random)).

:- use_module(model).
:- use_module(occupancy).
:- use_module(scoring).

search_best(TasksWithCands, Policy, InitialOcc, BaseAssignments,
            BestAssignments, BestUnplaced, BestHardPenalty, BestTotalPenalty) :-
    cfg(Policy, Cfg),
    get_time(T0),
    run_restarts(TasksWithCands, Policy, Cfg, T0, InitialOcc, BaseAssignments,
                 none, 1.0Inf,
                 BestAssignments, BestUnplaced, BestHardPenalty, BestTotalPenalty).

cfg(Policy, cfg{timeLimitMs:TL, restarts:MaxR, topK:TopK}) :-
    (   get_dict(search, Policy, S)
    ->  true
    ;   S = _{}
    ),
    (   get_dict(timeLimitMs, S, TL0) -> TL = TL0 ; TL = 1500 ),
    (   get_dict(restarts, S, R0) -> MaxR = R0 ; MaxR = 1000000 ),
    (   get_dict(topK, S, K0) -> TopK = K0 ; TopK = 3 ).

time_up(T0, TLms) :-
    get_time(T),
    ElapsedMs is (T - T0) * 1000.0,
    ElapsedMs >= TLms.

run_restarts(_Tasks, _Policy, Cfg, T0, _InitialOcc, _BaseAssignments,
             Best0, BestTotal0,
             BestA, BestU, BestHard, BestTotal) :-
    time_up(T0, Cfg.timeLimitMs),
    !,
    finalize_best(Best0, BestTotal0, BestA, BestU, BestHard, BestTotal).
run_restarts(Tasks, Policy, Cfg, T0, InitialOcc, BaseAssignments,
             Best0, BestTotal0,
             BestA, BestU, BestHard, BestTotal) :-
    (   Cfg.restarts =< 0
    ->  finalize_best(Best0, BestTotal0, BestA, BestU, BestHard, BestTotal)
    ;   run_once(Tasks, Policy, Cfg.topK, InitialOcc, BaseAssignments,
                 A1, U1, Hard1, Total1),
        (   Total1 < BestTotal0
        ->  Best1 = best(A1, U1, Hard1), BestTotal1 = Total1
        ;   Best1 = Best0, BestTotal1 = BestTotal0
        ),
        Cfg2 = Cfg.put(restarts, Cfg.restarts - 1),
        run_restarts(Tasks, Policy, Cfg2, T0, InitialOcc, BaseAssignments,
                     Best1, BestTotal1,
                     BestA, BestU, BestHard, BestTotal)
    ).

finalize_best(none, _Inf, [], [], 1.0Inf, 1.0Inf) :- !.
finalize_best(best(A,U,Hard), Total, A, U, Hard, Total).

run_once(TasksWithCands, Policy, TopK, InitialOcc, BaseAssignments,
         Assignments, Unplaced, HardPenalty, TotalPenalty) :-
    limits_params(Policy, Params),
    greedy_build(TasksWithCands, TopK, Params,
                 InitialOcc, [], [], 0.0,
                 AssignsRev, UnplacedRev, HardPenalty),
    reverse(AssignsRev, Assignments),
    reverse(UnplacedRev, Unplaced),
    append(BaseAssignments, Assignments, AllAssignments),
    soft_penalty(AllAssignments, Policy, SoftPenalty),
    TotalPenalty is HardPenalty + SoftPenalty.

greedy_build([], _TopK, _Params, _Occ, A, U, P, A, U, P).
greedy_build([tc(Task, Cands)|Rest], TopK, Params,
             Occ0, A0, U0, P0,
             A, U, P) :-
    (   pick_best_candidate(Cands, Task, Occ0, Params, TopK, Cand)
    ->  true
    ;   Cand = none
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
    (   K =< 0
    ->  Prefix = []
    ;   length(Prefix, K),
        append(Prefix, _, List)
    ->  true
    ;   Prefix = List
    ).

delta_limits(cand(_RoomCode, Day, _Start, _End, SlotIdx),
             task(_RequestId, _CourseCode, TeacherId, Groups, _ReqCap, _ReqItems),
             occ(_RoomOcc, TeacherOcc, GroupOcc),
             Params, Delta) :-
    slot_bit(SlotIdx, Bit),
    get_mask(TeacherOcc, teacher(Day, TeacherId), TMask0),
    TMask1 is TMask0 \/ Bit,
    delta_from_masks(TMask0, TMask1, Params.maxTDay, Params.maxTCons,
                     Params.slotsN, Params.wDay, Params.wCons, DT),
    delta_groups(Groups, Day, Bit, GroupOcc, Params, DG),
    Delta is DT + DG.

delta_groups([], _Day, _Bit, _GroupOcc, _Params, 0.0).
delta_groups([G|Gs], Day, Bit, GroupOcc, Params, Delta) :-
    get_mask(GroupOcc, group(Day, G), M0),
    M1 is M0 \/ Bit,
    delta_from_masks(M0, M1, Params.maxGDay, Params.maxGCons,
                     Params.slotsN, Params.wDay, Params.wCons, D1),
    delta_groups(Gs, Day, Bit, GroupOcc, Params, DRest),
    Delta is D1 + DRest.

apply_pick(none, Cands, _Task, Occ, A0, U0, P0, Occ, A0, U1, P1) :-
    member(unplaced(UnplacedPenalty, RequestId, CourseCode, Reason, _TeacherId, _Groups), Cands),
    U1 = [_{requestId:RequestId, courseCode:CourseCode, reason:Reason}|U0],
    P1 is P0 + UnplacedPenalty.
apply_pick(cand(RoomCode, Day, Start, End, SlotIdx), _Cands,
           task(RequestId, CourseCode, TeacherId, Groups, _ReqCap, _ReqItems),
           Occ0, A0, U0, P0,
           Occ1, [A|A0], U0, P0) :-
    put_candidate_occ(cand(RoomCode, Day, Start, End, SlotIdx),
                      task(RequestId, CourseCode, TeacherId, Groups, 0, []),
                      Occ0, Occ1),
    A = _{
        source:"GENERATED",
        requestId:RequestId,
        courseCode:CourseCode,
        teacherId:TeacherId,
        groups:Groups,
        roomCode:RoomCode,
        dayOfWeek:Day,
        startTime:Start,
        endTime:End,
        slotIndex:SlotIdx
    }.
