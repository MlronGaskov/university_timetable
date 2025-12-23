:- module(candidates, [
    expand_courses_by_hours/2,
    build_tasks/2,
    build_all_candidates/6,
    sort_tasks_mrv/2
]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(model).

expand_courses_by_hours([], []).
expand_courses_by_hours([Course|Rest], Expanded) :-
    Planned = Course.plannedHours,
    RequiredSlotsF is Planned / 2,
    RequiredSlots is max(1, ceiling(RequiredSlotsF)),
    findall(Course, between(1, RequiredSlots, _), Copies),
    expand_courses_by_hours(Rest, Tail),
    append(Copies, Tail, Expanded).

build_tasks(ExpandedCourses, Tasks) :-
    foldl(course_to_task_state, ExpandedCourses, state(_{}, []), state(_, Rev)),
    reverse(Rev, Tasks).

course_to_task_state(Course, state(Seen0, Acc0), state(Seen1, [Task|Acc0])) :-
    get_dict(id, Course, CourseId0),
    ensure_string(CourseId0, CIdStr),
    ensure_key(CIdStr, CIdKey),

    ( get_dict(CIdKey, Seen0, Count0) -> Count is Count0 + 1 ; Count = 1 ),
    put_dict(CIdKey, Seen0, Count, Seen1),

    get_dict(teacherId, Course, TeacherId0),
    ensure_string(TeacherId0, TeacherIdStr),

    get_dict(groupIds, Course, Groups0),
    ( is_list(Groups0) -> Groups = Groups0 ; Groups = [] ),

    get_dict(requiredRoomCapacity, Course, ReqCap),

    get_dict(equipmentRequirements, Course, ReqItems0),
    ( is_list(ReqItems0) -> ReqItems = ReqItems0 ; ReqItems = [] ),

    Task = task(CIdStr, TeacherIdStr, Groups, ReqCap, ReqItems, Count).

build_all_candidates(Tasks, Rooms0, Teachers0, SlotTemplates0, Policy, TasksWithCands) :-
    preprocess_rooms(Rooms0, Rooms),
    ensure_slot_templates_have_minutes(SlotTemplates0, SlotTemplates),
    build_teachers_index(Teachers0, SlotTemplates, TeachersById),
    findall(tc(Task, CandList),
        (
            member(Task, Tasks),
            build_candidates_for_task(Task, Rooms, TeachersById, SlotTemplates, Policy, CandList)
        ),
        TasksWithCands).

sort_tasks_mrv(TasksWithCands, Sorted) :-
    map_list_to_pairs(mrv_key, TasksWithCands, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

mrv_key(tc(_Task, Cands), N) :-
    include(is_real_cand, Cands, Real),
    length(Real, N).

is_real_cand(cand(_,_,_,_,_)).

preprocess_rooms(Rooms0, Rooms) :-
    maplist(preprocess_room, Rooms0, Rooms).

preprocess_room(Room0, Room) :-
    get_dict(items, Room0, Items0),
    ( is_list(Items0)
    -> maplist(norm_item_kv, Items0, KVs),
       kvs_to_items_map(KVs, _{}, ItemsMap)
    ;  ItemsMap = _{}
    ),
    put_dict(_{itemsMap: ItemsMap}, Room0, Room).

norm_item_kv(D, kv(KeyAtom, Qty)) :-
    get_dict(name, D, Name0),
    get_dict(quantity, D, Qty),
    ensure_string(Name0, NameStr),
    string_lower(NameStr, NameLower),
    ensure_key(NameLower, KeyAtom).

kvs_to_items_map([], Map, Map).
kvs_to_items_map([kv(K,V)|Rest], Map0, Map) :-
    ( get_dict(K, Map0, Old) -> New is Old + V ; New = V ),
    put_dict(K, Map0, New, Map1),
    kvs_to_items_map(Rest, Map1, Map).

ensure_slot_templates_have_minutes([], []).
ensure_slot_templates_have_minutes([S0|Rest0], [S|Rest]) :-
    ( S0 = slot_t(Day, Start, End, SlotIdx, _SM, _EM)
    -> S = S0
    ; S0 = slot_t(Day, Start, End, SlotIdx),
      time_minutes(Start, SM),
      time_minutes(End, EM),
      S = slot_t(Day, Start, End, SlotIdx, SM, EM)
    ),
    ensure_slot_templates_have_minutes(Rest0, Rest).

build_teachers_index(Teachers, SlotTemplates, Dict) :-
    foldl(put_teacher(SlotTemplates), Teachers, _{}, Dict).

put_teacher(SlotTemplates, T, D0, D1) :-
    get_dict(teacherId, T, Id0),
    ensure_string(Id0, IdStr),
    ensure_key(IdStr, IdKey),
    teacher_avail_by_day(T, AvailByDay),
    include(slot_allowed(AvailByDay), SlotTemplates, AllowedTemplates),
    put_dict(IdKey, D0, teacher_info(AllowedTemplates), D1).

teacher_avail_by_day(Teacher, AvailByDay) :-
    get_dict(preferredWorkingHours, Teacher, Hours0),
    ( is_list(Hours0) -> Hours = Hours0 ; Hours = [] ),
    foldl(add_hour_interval, Hours, _{}, AvailByDay).

add_hour_interval(H, D0, D1) :-
    get_dict(day, H, Day0),
    ensure_string(Day0, DayStr),
    ensure_key(DayStr, DayKey),
    get_dict(startTime, H, WS0),
    get_dict(endTime, H, WE0),
    time_minutes(WS0, WSM),
    time_minutes(WE0, WEM),
    ( get_dict(DayKey, D0, Ints0) -> true ; Ints0 = [] ),
    put_dict(DayKey, D0, [int(WSM, WEM)|Ints0], D1).

slot_allowed(AvailByDay, slot_t(DayStr, _Start, _End, _SlotIdx, SM, EM)) :-
    ensure_key(DayStr, DayKey),
    get_dict(DayKey, AvailByDay, Ints),
    member(int(WS, WE), Ints),
    SM >= WS,
    EM =< WE,
    !.

build_candidates_for_task(
    task(CIdStr, TeacherIdStr, Groups, ReqCap, ReqItems, Instance),
    Rooms, TeachersById, _SlotTemplates, Policy, CandListOut
) :-
    normalize_requirements(ReqItems, NormReqs),
    include(room_fits_req(ReqCap, NormReqs), Rooms, OkRooms0),
    max_rooms_per_task(Policy, MaxRooms),
    limit_rooms_by_capacity(OkRooms0, ReqCap, MaxRooms, OkRooms),

    ( ensure_key(TeacherIdStr, TeacherKey),
      get_dict(TeacherKey, TeachersById, teacher_info(AllowedTemplates))
    -> true
    ;  AllowedTemplates = []
    ),

    findall(cand(RoomCode, Day, Start, End, SlotIdx),
        (
            member(Room, OkRooms),
            get_dict(roomCode, Room, RoomCode0),
            ensure_string(RoomCode0, RoomCode),
            member(slot_t(Day, Start, End, SlotIdx, _SM, _EM), AllowedTemplates)
        ),
        CandList0),

    unplaced_reason(_CIdStr, Instance, Reason),
    get_dict(weights, Policy, W),
    get_dict(hard, W, Hard),
    get_dict(unplacedPair, Hard, UnplacedPenalty),

    append(CandList0,
           [unplaced(UnplacedPenalty, CIdStr, Reason, TeacherIdStr, Groups)],
           CandListOut).

max_rooms_per_task(Policy, MaxRooms) :-
    ( get_dict(search, Policy, S),
      get_dict(maxRoomsPerTask, S, M)
    -> MaxRooms = M
    ;  MaxRooms = 20
    ).

limit_rooms_by_capacity(Rooms, ReqCap, MaxRooms, Limited) :-
    map_list_to_pairs(room_slack(ReqCap), Rooms, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, SortedRooms),
    take_prefix(SortedRooms, MaxRooms, Limited).

room_slack(ReqCap, Room, Slack) :-
    get_dict(capacity, Room, Cap),
    Slack is Cap - ReqCap.

take_prefix(List, N, Prefix) :-
    ( N =< 0 -> Prefix = []
    ; length(Prefix, N),
      append(Prefix, _, List)
    -> true
    ; Prefix = List
    ).

unplaced_reason(_CourseId, Instance, Reason) :-
    format(string(Reason), "Unplaced (occurrence ~w)", [Instance]).

normalize_requirements(ReqItems0, NormReqs) :-
    ( is_list(ReqItems0) -> ReqItems = ReqItems0 ; ReqItems = [] ),
    maplist(norm_req_item, ReqItems, NormReqs).

norm_req_item(D, item(KeyAtom, Needed)) :-
    get_dict(name, D, Name0),
    get_dict(quantity, D, Needed),
    ensure_string(Name0, NameStr),
    string_lower(NameStr, NameLower),
    ensure_key(NameLower, KeyAtom).

room_fits_req(ReqCap, NormReqs, Room) :-
    get_dict(capacity, Room, Cap),
    Cap >= ReqCap,
    room_satisfies_items(Room, NormReqs).

room_satisfies_items(_Room, []) :- !.
room_satisfies_items(Room, Requirements) :-
    ( get_dict(itemsMap, Room, ItemsMap) -> true ; ItemsMap = _{} ),
    forall(
        member(item(KeyAtom, Needed), Requirements),
        ( get_dict(KeyAtom, ItemsMap, Have), Have >= Needed )
    ).
