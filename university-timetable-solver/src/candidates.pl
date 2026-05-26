:- module(candidates, [
    build_tasks_from_requests/2,
    build_all_candidates/6,
    sort_tasks_mrv/2,
    teacher_allows_assignment/3
]).

:- use_module(library(lists)).
:- use_module(library(apply)).

:- use_module(model).

build_tasks_from_requests(Requests, Tasks) :-
    maplist(request_to_task, Requests, Tasks).

request_to_task(Req, task(RequestId, CourseCode, TeacherId, Groups, ReqCap, ReqItems)) :-
    required_any(Req, [requestId, id], RequestId0, "slot request id"),
    required_any(Req, [courseCode, courseId], CourseCode0, "courseCode"),
    required_any(Req, [teacherId], TeacherId0, "teacherId"),
    required_any(Req, [groupIds, groups], Groups0, "groupIds"),
    dict_get_any_default(Req, [requiredRoomCapacity], 0, ReqCap),
    dict_get_any_default(Req, [equipmentRequirements, requiredItems], [], ReqItems0),

    ensure_string(RequestId0, RequestId),
    ensure_string(CourseCode0, CourseCode),
    ensure_string(TeacherId0, TeacherId),
    normalize_nonempty_string_list(Groups0, Groups, "groupIds"),
    ( is_list(ReqItems0) -> ReqItems = ReqItems0 ; ReqItems = [] ).

required_any(Dict, Keys, Value, HumanName) :-
    (   dict_get_any(Dict, Keys, Value)
    ->  true
    ;   format(string(Msg), "Missing field in slotsToPlace item: ~s", [HumanName]),
        throw(error(invalid_request(Msg), _))
    ).

normalize_string_list([], []).
normalize_string_list([X|Xs], [S|Ss]) :-
    ensure_string(X, S),
    normalize_string_list(Xs, Ss).

normalize_nonempty_string_list(Value, Strings, HumanName) :-
    (   is_list(Value)
    ->  normalize_string_list(Value, Strings0),
        exclude(blank_string, Strings0, Strings1),
        sort(Strings1, Strings),
        (   Strings = []
        ->  format(string(Msg), "Field in slotsToPlace item must be a non-empty list: ~s", [HumanName]),
            throw(error(invalid_request(Msg), _))
        ;   true
        )
    ;   format(string(Msg), "Field in slotsToPlace item must be a list: ~s", [HumanName]),
        throw(error(invalid_request(Msg), _))
    ).

blank_string(S) :-
    normalize_space(string(T), S),
    T = "".

build_all_candidates(Tasks, Rooms0, Teachers0, SlotTemplates0, Policy, TasksWithCands) :-
    preprocess_rooms(Rooms0, Rooms),
    ensure_slot_templates_have_minutes(SlotTemplates0, SlotTemplates),
    build_teachers_index(Teachers0, SlotTemplates, TeachersById),
    findall(tc(Task, CandList),
        (
            member(Task, Tasks),
            build_candidates_for_task(Task, Rooms, TeachersById, Policy, CandList)
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
    required_room_code(Room0, RoomCode),
    (   get_dict(capacity, Room0, Cap0)
    ->  Cap = Cap0
    ;   Cap = 0
    ),
    dict_get_any_default(Room0, [items, equipment], [], Items0),
    (   is_list(Items0)
    ->  maplist(norm_item_kv, Items0, KVs),
        kvs_to_items_map(KVs, _{}, ItemsMap)
    ;   ItemsMap = _{}
    ),
    put_dict(_{roomCode:RoomCode, capacity:Cap, itemsMap:ItemsMap}, Room0, Room).

required_room_code(Room, RoomCode) :-
    (   dict_get_any(Room, [roomCode, code], RoomCode0)
    ->  ensure_string(RoomCode0, RoomCode)
    ;   throw(error(invalid_request("Missing field in rooms item: roomCode"), _))
    ).

norm_item_kv(D, kv(KeyAtom, Qty)) :-
    dict_get_any(D, [name, itemName], Name0),
    dict_get_any_default(D, [quantity, qty, itemQty], 1, Qty),
    ensure_string(Name0, NameStr),
    string_lower(NameStr, NameLower),
    ensure_key(NameLower, KeyAtom).

kvs_to_items_map([], Map, Map).
kvs_to_items_map([kv(K,V)|Rest], Map0, Map) :-
    (   get_dict(K, Map0, Old)
    ->  New is Old + V
    ;   New = V
    ),
    put_dict(K, Map0, New, Map1),
    kvs_to_items_map(Rest, Map1, Map).

ensure_slot_templates_have_minutes([], []).
ensure_slot_templates_have_minutes([S0|Rest0], [S|Rest]) :-
    (   S0 = slot_t(Day, Start, End, SlotIdx, _SM, _EM)
    ->  S = S0
    ;   S0 = slot_t(Day, Start, End, SlotIdx),
        time_minutes(Start, SM),
        time_minutes(End, EM),
        S = slot_t(Day, Start, End, SlotIdx, SM, EM)
    ),
    ensure_slot_templates_have_minutes(Rest0, Rest).

build_teachers_index(Teachers, SlotTemplates, Dict) :-
    foldl(put_teacher(SlotTemplates), Teachers, _{}, Dict).

put_teacher(SlotTemplates, T, D0, D1) :-
    (   dict_get_any(T, [teacherId, id], Id0)
    ->  ensure_string(Id0, IdStr),
        ensure_key(IdStr, IdKey),
        teacher_avail_by_day(T, AvailByDay),
        include(slot_allowed(AvailByDay), SlotTemplates, AllowedTemplates),
        put_dict(IdKey, D0, teacher_info(AllowedTemplates), D1)
    ;   D1 = D0
    ).

teacher_avail_by_day(Teacher, AvailByDay) :-
    dict_get_any_default(Teacher, [preferredWorkingHours, workingHours], [], Hours0),
    (   is_list(Hours0) -> Hours = Hours0 ; Hours = [] ),
    foldl(add_hour_interval, Hours, _{}, AvailByDay).

add_hour_interval(H, D0, D1) :-
    dict_get_any(H, [day, dayOfWeek], Day0),
    ensure_string(Day0, DayStr),
    ensure_key(DayStr, DayKey),
    get_dict(startTime, H, WS0),
    get_dict(endTime, H, WE0),
    time_minutes(WS0, WSM),
    time_minutes(WE0, WEM),
    (   get_dict(DayKey, D0, Ints0)
    ->  true
    ;   Ints0 = []
    ),
    put_dict(DayKey, D0, [int(WSM, WEM)|Ints0], D1).

slot_allowed(AvailByDay, slot_t(DayStr, _Start, _End, _SlotIdx, SM, EM)) :-
    ensure_key(DayStr, DayKey),
    get_dict(DayKey, AvailByDay, Ints),
    member(int(WS, WE), Ints),
    SM >= WS,
    EM =< WE,
    !.

teacher_allows_assignment(Teachers, SlotTemplates, A) :-
    build_teachers_index(Teachers, SlotTemplates, TeachersById),
    ensure_key(A.teacherId, TeacherKey),
    get_dict(TeacherKey, TeachersById, teacher_info(AllowedTemplates)),
    member(slot_t(A.dayOfWeek, A.startTime, A.endTime, A.slotIndex, _SM, _EM), AllowedTemplates),
    !.

build_candidates_for_task(
    task(RequestId, CourseCode, TeacherId, Groups, ReqCap, ReqItems),
    Rooms, TeachersById, Policy, CandListOut
) :-
    normalize_requirements(ReqItems, NormReqs),
    include(room_fits_req(ReqCap, NormReqs), Rooms, OkRooms0),
    max_rooms_per_task(Policy, MaxRooms),
    limit_rooms_by_capacity(OkRooms0, ReqCap, MaxRooms, OkRooms),

    (   ensure_key(TeacherId, TeacherKey),
        get_dict(TeacherKey, TeachersById, teacher_info(AllowedTemplates))
    ->  true
    ;   AllowedTemplates = []
    ),

    findall(cand(RoomCode, Day, Start, End, SlotIdx),
        (
            member(Room, OkRooms),
            get_dict(roomCode, Room, RoomCode),
            member(slot_t(Day, Start, End, SlotIdx, _SM, _EM), AllowedTemplates)
        ),
        CandList0),

    get_dict(weights, Policy, W),
    get_dict(hard, W, Hard),
    get_dict(unplacedPair, Hard, UnplacedPenalty),
    format(string(Reason), "Unplaced slot request ~s for course ~s", [RequestId, CourseCode]),
    append(CandList0,
           [unplaced(UnplacedPenalty, RequestId, CourseCode, Reason, TeacherId, Groups)],
           CandListOut).

max_rooms_per_task(Policy, MaxRooms) :-
    (   get_dict(search, Policy, S),
        get_dict(maxRoomsPerTask, S, M)
    ->  MaxRooms = M
    ;   MaxRooms = 20
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
    (   N =< 0
    ->  Prefix = []
    ;   length(Prefix, N),
        append(Prefix, _, List)
    ->  true
    ;   Prefix = List
    ).

normalize_requirements(ReqItems0, NormReqs) :-
    (   is_list(ReqItems0) -> ReqItems = ReqItems0 ; ReqItems = [] ),
    maplist(norm_req_item, ReqItems, NormReqs).

norm_req_item(D, item(KeyAtom, Needed)) :-
    dict_get_any(D, [name, itemName], Name0),
    dict_get_any_default(D, [quantity, qty, itemQty], 1, Needed),
    ensure_string(Name0, NameStr),
    string_lower(NameStr, NameLower),
    ensure_key(NameLower, KeyAtom).

room_fits_req(ReqCap, NormReqs, Room) :-
    get_dict(capacity, Room, Cap),
    Cap >= ReqCap,
    room_satisfies_items(Room, NormReqs).

room_satisfies_items(_Room, []) :- !.
room_satisfies_items(Room, Requirements) :-
    (   get_dict(itemsMap, Room, ItemsMap)
    ->  true
    ;   ItemsMap = _{}
    ),
    forall(
        member(item(KeyAtom, Needed), Requirements),
        (get_dict(KeyAtom, ItemsMap, Have), Have >= Needed)
    ).
