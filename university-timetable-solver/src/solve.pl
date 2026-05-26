:- module(solve, [solve_request/2]).

:- use_module(library(lists)).
:- use_module(validate).
:- use_module(policy).
:- use_module(model).
:- use_module(slots).
:- use_module(candidates).
:- use_module(occupancy).
:- use_module(search).
:- use_module(scoring).

solve_request(Body, Response) :-
    ensure_required(Body),

    get_dict(semester, Body, Semester),
    get_dict(rooms, Body, Rooms),
    get_dict(teachers, Body, Teachers),
    get_dict(policy, Body, PolicyIn),
    get_dict(fixedSlots, Body, FixedSlots0),
    get_dict(slotsToPlace, Body, SlotsToPlace0),

    required_semester_dates(Semester, StartDate, EndDate),

    parse_policy(PolicyIn, Policy),
    build_slot_templates(Policy.grid, Policy.limits, SlotTemplates),

    normalize_fixed_slots(FixedSlots0, SlotTemplates, FixedAssignments),
    validate_fixed_refs(FixedAssignments, Rooms, Teachers),
    fixed_conflicts(FixedAssignments, Conflicts),
    throw_if_fixed_conflicts(Conflicts),
    fixed_warnings(FixedAssignments, Teachers, SlotTemplates, FixedWarnings),

    empty_occ(EmptyOcc),
    build_occ_from_assignments(FixedAssignments, EmptyOcc, InitialOcc),

    build_tasks_from_requests(SlotsToPlace0, Tasks),
    build_all_candidates(Tasks, Rooms, Teachers, SlotTemplates, Policy, TasksWithCands0),
    sort_tasks_mrv(TasksWithCands0, TasksWithCands),

    search_best(TasksWithCands, Policy, InitialOcc, FixedAssignments,
                NewAssignments, Unplaced, HardPenalty, _BestTotal),

    append(FixedAssignments, NewAssignments, AllAssignments),
    hard_conflicts(AllAssignments, GeneratedConflicts),
    throw_if_generated_conflicts(GeneratedConflicts),
    soft_penalty(AllAssignments, Policy, SoftPenalty),
    TotalPenalty is HardPenalty + SoftPenalty,
    score_from_penalty(TotalPenalty, Score),

    public_generated_slots(NewAssignments, StartDate, EndDate, PlacedSlots),
    public_fixed_slots(FixedAssignments, StartDate, EndDate, PublicFixedSlots),
    append(PublicFixedSlots, PlacedSlots, ScheduleSlots),

    Response = _{
        placedSlots:PlacedSlots,
        fixedSlots:PublicFixedSlots,
        scheduleSlots:ScheduleSlots,
        unplaced:Unplaced,
        warnings:_{fixedSlots:FixedWarnings},
        evaluationPenalty:TotalPenalty,
        evaluationScore:Score
    }.

required_semester_dates(Semester, StartDate, EndDate) :-
    (   dict_get_any(Semester, [startAt, start_at], Start0)
    ->  true
    ;   throw(error(invalid_request("Missing semester.startAt"), _))
    ),
    (   dict_get_any(Semester, [endAt, end_at], End0)
    ->  true
    ;   throw(error(invalid_request("Missing semester.endAt"), _))
    ),
    instant_date(Start0, StartDate),
    instant_date(End0, EndDate).

normalize_fixed_slots(FixedSlots0, SlotTemplates, FixedAssignments) :-
    normalize_fixed_slots_(FixedSlots0, SlotTemplates, 1, FixedAssignments).

normalize_fixed_slots_([], _SlotTemplates, _N, []).
normalize_fixed_slots_([S|Rest], SlotTemplates, N, [A|As]) :-
    fixed_slot_to_assignment(S, SlotTemplates, N, A),
    N1 is N + 1,
    normalize_fixed_slots_(Rest, SlotTemplates, N1, As).

fixed_slot_to_assignment(S, SlotTemplates, N, A) :-
    optional_fixed_slot_id(S, N, SlotId),
    required_any_fixed(S, [courseCode, courseId], CourseCode0, "courseCode"),
    required_any_fixed(S, [teacherId], TeacherId0, "teacherId"),
    required_any_fixed(S, [roomCode], RoomCode0, "roomCode"),
    required_any_fixed(S, [dayOfWeek, day], Day0, "dayOfWeek"),
    required_any_fixed(S, [startTime], Start0, "startTime"),
    required_any_fixed(S, [endTime], End0, "endTime"),
    required_any_fixed(S, [groupIds, groups], Groups0, "groupIds"),

    ensure_string(CourseCode0, CourseCode),
    ensure_string(TeacherId0, TeacherId),
    ensure_string(RoomCode0, RoomCode),
    ensure_string(Day0, Day),
    ensure_string(Start0, Start),
    ensure_string(End0, End),
    normalize_nonempty_fixed_groups(Groups0, Groups),

    (   slot_index_for_time(SlotTemplates, Day, Start, End, SlotIdx, _)
    ->  true
    ;   format(string(Msg),
               "Fixed slot ~s does not match policy grid/time bounds: ~s ~s-~s",
               [SlotId, Day, Start, End]),
        throw(error(invalid_request(Msg), _))
    ),

    A = _{
        source:"FIXED",
        slotId:SlotId,
        courseCode:CourseCode,
        teacherId:TeacherId,
        groups:Groups,
        roomCode:RoomCode,
        dayOfWeek:Day,
        startTime:Start,
        endTime:End,
        slotIndex:SlotIdx
    }.

optional_fixed_slot_id(S, N, SlotId) :-
    (   dict_get_any(S, [slotId, id], SlotId0)
    ->  ensure_string(SlotId0, SlotId)
    ;   format(string(SlotId), "fixed-~d", [N])
    ).

required_any_fixed(Dict, Keys, Value, HumanName) :-
    (   dict_get_any(Dict, Keys, Value)
    ->  true
    ;   format(string(Msg), "Missing field in fixedSlots item: ~s", [HumanName]),
        throw(error(invalid_request(Msg), _))
    ).

normalize_string_list([], []).
normalize_string_list([X|Xs], [S|Ss]) :-
    ensure_string(X, S),
    normalize_string_list(Xs, Ss).

normalize_nonempty_fixed_groups(Value, Groups) :-
    (   is_list(Value)
    ->  normalize_string_list(Value, Groups0),
        exclude(blank_string, Groups0, Groups1),
        sort(Groups1, Groups),
        (   Groups = []
        ->  throw(error(invalid_request("Field in fixedSlots item must be a non-empty list: groupIds"), _))
        ;   true
        )
    ;   throw(error(invalid_request("Field in fixedSlots item must be a list: groupIds"), _))
    ).

blank_string(S) :-
    normalize_space(string(T), S),
    T = "".

validate_fixed_refs(FixedAssignments, Rooms, Teachers) :-
    collect_room_codes(Rooms, RoomCodes),
    collect_teacher_ids(Teachers, TeacherIds),
    findall(V, fixed_ref_violation(FixedAssignments, RoomCodes, TeacherIds, V), Violations),
    (   Violations = []
    ->  true
    ;   Violations = [First|_],
        violation_message(First, Msg),
        throw(error(invalid_request(Msg), _))
    ).

collect_room_codes(Rooms, Codes) :-
    findall(Code,
        (
            member(R, Rooms),
            dict_get_any(R, [roomCode, code], Code0),
            ensure_string(Code0, Code)
        ),
        Codes).

collect_teacher_ids(Teachers, Ids) :-
    findall(Id,
        (
            member(T, Teachers),
            dict_get_any(T, [teacherId, id], Id0),
            ensure_string(Id0, Id)
        ),
        Ids).

fixed_ref_violation(FixedAssignments, RoomCodes, _TeacherIds, V) :-
    member(A, FixedAssignments),
    \+ memberchk(A.roomCode, RoomCodes),
    V = _{type:"UNKNOWN_ROOM", slotId:A.slotId, roomCode:A.roomCode}.
fixed_ref_violation(FixedAssignments, _RoomCodes, TeacherIds, V) :-
    member(A, FixedAssignments),
    \+ memberchk(A.teacherId, TeacherIds),
    V = _{type:"UNKNOWN_TEACHER", slotId:A.slotId, teacherId:A.teacherId}.

violation_message(V, Msg) :-
    (   V.type = "UNKNOWN_ROOM"
    ->  format(string(Msg), "Fixed slot ~s references unknown room ~s", [V.slotId, V.roomCode])
    ;   V.type = "UNKNOWN_TEACHER"
    ->  format(string(Msg), "Fixed slot ~s references unknown teacher ~s", [V.slotId, V.teacherId])
    ;   format(string(Msg), "Invalid fixed slot: ~w", [V])
    ).

throw_if_fixed_conflicts([]) :- !.
throw_if_fixed_conflicts(Conflicts) :-
    throw(error(fixed_slots_conflict(Conflicts), _)).

throw_if_generated_conflicts([]) :- !.
throw_if_generated_conflicts(Conflicts) :-
    throw(error(generated_slots_conflict(Conflicts), _)).

fixed_warnings(FixedAssignments, Teachers, SlotTemplates, Warnings) :-
    findall(W,
        (
            member(A, FixedAssignments),
            \+ teacher_allows_assignment(Teachers, SlotTemplates, A),
            W = _{
                type:"FIXED_SLOT_OUTSIDE_TEACHER_WORKING_HOURS",
                slotId:A.slotId,
                teacherId:A.teacherId,
                dayOfWeek:A.dayOfWeek,
                startTime:A.startTime,
                endTime:A.endTime
            }
        ),
        Warnings).

public_generated_slots(Assignments, StartDate, EndDate, PublicSlots) :-
    findall(Slot,
        (
            member(A, Assignments),
            Slot = _{
                source:"GENERATED",
                requestId:A.requestId,
                courseCode:A.courseCode,
                teacherId:A.teacherId,
                groupIds:A.groups,
                roomCode:A.roomCode,
                dayOfWeek:A.dayOfWeek,
                startTime:A.startTime,
                endTime:A.endTime,
                validFrom:StartDate,
                validUntil:EndDate,
                weekPattern:"EVERY_WEEK"
            }
        ),
        PublicSlots).

public_fixed_slots(Assignments, StartDate, EndDate, PublicSlots) :-
    findall(Slot,
        (
            member(A, Assignments),
            Slot = _{
                source:"FIXED",
                slotId:A.slotId,
                courseCode:A.courseCode,
                teacherId:A.teacherId,
                groupIds:A.groups,
                roomCode:A.roomCode,
                dayOfWeek:A.dayOfWeek,
                startTime:A.startTime,
                endTime:A.endTime,
                validFrom:StartDate,
                validUntil:EndDate,
                weekPattern:"EVERY_WEEK"
            }
        ),
        PublicSlots).
