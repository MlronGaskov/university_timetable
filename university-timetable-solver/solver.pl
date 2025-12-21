:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(apply)).

:- http_handler(root(solve), solve_handler, []).

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

solve_handler(Request) :-
    catch(
        (
            http_read_json_dict(Request, Body, []),
            solve_request(Body, Response),
            reply_json_dict(Response)
        ),
        Error,
        reply_error(Error)
    ).

reply_error(error(invalid_request(Message), _)) :-
    !,
    reply_json_dict(_{error: Message}, [status(400)]).
reply_error(Error) :-
    message_to_string(Error, Msg),
    reply_json_dict(_{error: Msg}, [status(500)]).

solve_request(Body, _{slots: PublicSlots, evaluationScore: Score}) :-
    ensure_required(Body),
    get_dict(semester, Body, Semester),
    get_dict(courses, Body, Courses0),
    get_dict(rooms, Body, Rooms),
    get_dict(teachers, Body, Teachers),
    get_dict(policy, Body, Policy),

    instant_date(Semester.startAt, StartDate),
    instant_date(Semester.endAt, EndDate),

    parse_grid(Policy.gridJson, Grid),

    slots_from_grid(Grid, SlotTemplates),

    expand_courses_by_hours(Courses0, ExpandedCourses),

    teacher_map(Teachers, TeacherMap),

    assign_courses(
        ExpandedCourses,
        Rooms,
        TeacherMap,
        SlotTemplates,
        StartDate,
        EndDate,
        MetaSlots
    ),

    maplist(meta_to_public_slot, MetaSlots, PublicSlots),
    length(ExpandedCourses, Requested),
    length(PublicSlots, Produced),
    (Requested =:= 0 -> Score = 0.0 ; Score is Produced / Requested).

ensure_required(Body) :-
    (get_dict(semester, Body, _) -> true ; throw(error(invalid_request("Missing field: semester"), _))),
    (get_dict(courses, Body, Courses), is_list(Courses) -> true ; throw(error(invalid_request("Missing or invalid field: courses"), _))),
    (get_dict(rooms, Body, Rooms), is_list(Rooms) -> true ; throw(error(invalid_request("Missing or invalid field: rooms"), _))),
    (get_dict(teachers, Body, Teachers), is_list(Teachers) -> true ; throw(error(invalid_request("Missing or invalid field: teachers"), _))),
    (get_dict(policy, Body, _) -> true ; throw(error(invalid_request("Missing field: policy"), _))).

instant_date(InstantAtom, DateString) :-
    ensure_string(InstantAtom, InstantStr),
    ( sub_string(InstantStr, 0, 10, _, DateString)
    -> true
    ; DateString = InstantStr
    ).

parse_grid(GridJsonAtom, GridDict) :-
    ensure_string(GridJsonAtom, GridJsonString),
    atom_string(GridAtom, GridJsonString),
    ( catch(atom_json_dict(GridAtom, ParsedDict, []), _, fail),
      get_dict(slots, ParsedDict, Slots),
      get_dict(days, ParsedDict, Days),
      is_list(Slots),
      is_list(Days)
    -> GridDict = ParsedDict
    ;  default_grid(GridDict)
    ).

default_grid(_{
    days: ["MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY"],
    slots: [
        _{start:"09:00", end:"10:30"},
        _{start:"10:40", end:"12:10"},
        _{start:"12:30", end:"14:00"},
        _{start:"14:10", end:"15:40"},
        _{start:"15:50", end:"17:20"},
        _{start:"17:30", end:"19:00"}
    ],
    weekPattern: "EVERY_WEEK"
}).

slots_from_grid(Grid, Slots) :-
    get_dict(days, Grid, Days),
    get_dict(slots, Grid, TimeSlots),
    length(Days, NumDays),
    length(TimeSlots, NumSlots),
    % Generate slots with day distribution priority
    % First all days with EVERY_WEEK, then ODD/EVEN as fallback
    findall(slot(Day, Start, End, Pattern),
            (member(S, TimeSlots), 
             get_dict(start, S, Start), 
             get_dict(end, S, End),
             member(Day, Days),
             member(Pattern, ["EVERY_WEEK", "ODD_WEEKS", "EVEN_WEEKS"])),
            Slots).

expand_courses_by_hours([], []).
expand_courses_by_hours([Course|Rest], Expanded) :-
    Planned = Course.plannedHours,
    % plannedHours is in academic hours (45 min each)
    % 1 pair = 2 academic hours = 90 min
    % So RequiredSlots = plannedHours / 2
    RequiredSlotsF is Planned / 2,
    RequiredSlots is max(1, ceiling(RequiredSlotsF)),
    findall(Course, between(1, RequiredSlots, _), Copies),
    expand_courses_by_hours(Rest, TailExpanded),
    append(Copies, TailExpanded, Expanded).

teacher_map(List, Map) :-
    findall(Id-Teacher,
            (member(Teacher, List),
             get_dict(teacherId, Teacher, Id)),
            Map).

assign_courses(Courses, Rooms, TeacherMap, SlotTemplates, StartDate, EndDate, SlotsOut) :-
    foldl(assign_course(Rooms, TeacherMap, SlotTemplates, StartDate, EndDate), Courses, [], SlotsOut).

assign_course(Rooms, TeacherMap, SlotTemplates, StartDate, EndDate, Course, Acc, [MetaSlot|Acc]) :-
    get_dict(id, Course, CourseId),
    get_dict(teacherId, Course, TeacherId),
    get_dict(groupIds, Course, GroupIds0),
    (is_list(GroupIds0) -> GroupIds = GroupIds0 ; GroupIds = []),
    select_room(Rooms, Course, Room),
    choose_slot(SlotTemplates, TeacherMap, TeacherId, GroupIds, Room, Acc, SlotTemplate),
    SlotTemplate = slot(Day, StartTime, EndTime, WeekPattern),
    MetaSlot = _{
        courseId: CourseId,
        roomCode: Room.roomCode,
        dayOfWeek: Day,
        startTime: StartTime,
        endTime: EndTime,
        validFrom: StartDate,
        validUntil: EndDate,
        weekPattern: WeekPattern,
        teacherId: TeacherId,
        groups: GroupIds
    }.

assign_course(_, _, _, _, _, Course, _, _) :-
    get_dict(id, Course, CourseId),
    format(string(Msg), "Cannot place course ~w", [CourseId]),
    throw(error(invalid_request(Msg), _)).

select_room(Rooms, Course, Room) :-
    get_dict(requiredRoomCapacity, Course, RequiredCapacity),
    get_dict(equipmentRequirements, Course, ReqItems0),
    (is_list(ReqItems0) -> ReqItems = ReqItems0 ; ReqItems = []),
    member(Room, Rooms),
    get_dict(capacity, Room, Cap),
    Cap >= RequiredCapacity,
    room_satisfies_items(Room, ReqItems),
    !.

room_satisfies_items(Room, Requirements) :-
    get_dict(items, Room, Items0), (is_list(Items0) -> Items = Items0 ; Items = []),
    maplist(downcase_item, Items, NormItems),
    maplist(downcase_item, Requirements, NormReqs),
    forall(member(R, NormReqs), item_available(R, NormItems)).

downcase_item(Item, item(NameLower, Qty)) :-
    get_dict(name, Item, Name),
    get_dict(quantity, Item, Qty),
    ensure_string(Name, NameStr),
    string_lower(NameStr, Lower),
    NameLower = Lower.

item_available(item(Name, NeededQty), Items) :-
    include(same_item(Name), Items, Matches),
    total_quantity(Matches, Total),
    Total >= NeededQty.

same_item(Name, item(Name0, _)) :-
    Name0 == Name.

total_quantity(Items, Total) :-
    foldl(sum_qty, Items, 0, Total).

sum_qty(item(_, Q), Acc, New) :- New is Acc + Q.

choose_slot(SlotTemplates, TeacherMap, TeacherId, GroupIds, Room, Existing, Slot) :-
    member(Slot, SlotTemplates),
    Slot = slot(Day, Start, End, WeekPattern),
    time_minutes(Start, StartM),
    time_minutes(End, EndM),
    teacher_ok(TeacherMap, TeacherId, Day, StartM, EndM),
    no_conflicts(Existing, Room, TeacherId, GroupIds, Day, StartM, EndM, WeekPattern).

teacher_ok(TeacherMap, TeacherId, Day, StartM, EndM) :-
    ( member(TeacherId-Teacher, TeacherMap)
    -> ( get_dict(preferredWorkingHours, Teacher, Intervals),
         Intervals \= [],
         member(Interval, Intervals),
         get_dict(day, Interval, Day),
         time_minutes(Interval.startTime, S),
         time_minutes(Interval.endTime, E),
         StartM >= S,
         EndM =< E
       )
    ;  true
    ).
teacher_ok(TeacherMap, TeacherId, _, _, _) :-
    % No matching interval – reject only when preferences exist
    member(TeacherId-Teacher, TeacherMap),
    get_dict(preferredWorkingHours, Teacher, Intervals),
    Intervals \= [],
    !,
    fail.
teacher_ok(_, _, _, _, _).

no_conflicts([], _, _, _, _, _, _, _).
no_conflicts([S|Rest], Room, TeacherId, GroupIds, Day, StartM, EndM, WeekPattern) :-
    get_dict(dayOfWeek, S, DayS),
    (DayS \== Day ->
        true
    ;
        time_minutes(S.startTime, SStart),
        time_minutes(S.endTime, SEnd),
        ( \+ times_overlap(StartM, EndM, SStart, SEnd)
        -> true
        ;  % Times overlap - check if week patterns allow coexistence
           get_dict(weekPattern, S, ExistingPattern),
           ( \+ week_patterns_conflict(WeekPattern, ExistingPattern)
           -> true  % ODD vs EVEN - no conflict
           ;  % Same week pattern or EVERY_WEEK involved - check other constraints
              get_dict(roomCode, S, RoomCode),
              ensure_string(RoomCode, ExistingRoomCode),
              get_dict(roomCode, Room, RoomCodeNewRaw),
              ensure_string(RoomCodeNewRaw, RoomCodeNew),
              ExistingRoomCode \== RoomCodeNew,
              get_dict(teacherId, S, TId),
              ensure_string(TId, ExistingTeacher),
              ensure_string(TeacherId, TeacherIdStr),
              ExistingTeacher \== TeacherIdStr,
              get_dict(groups, S, ExistingGroups),
              disjoint_groups(GroupIds, ExistingGroups)
           )
        )
    ),
    no_conflicts(Rest, Room, TeacherId, GroupIds, Day, StartM, EndM, WeekPattern).

disjoint_groups([], _).
disjoint_groups([G|Rest], Groups) :-
    \+ member(G, Groups),
    disjoint_groups(Rest, Groups).

times_overlap(Start1, End1, Start2, End2) :-
    Start1 < End2,
    Start2 < End1.

% Check if two week patterns can coexist in the same time slot
% ODD_WEEKS and EVEN_WEEKS don't conflict with each other
week_patterns_conflict(P1, P2) :-
    ensure_string(P1, Pattern1),
    ensure_string(P2, Pattern2),
    \+ (Pattern1 = "ODD_WEEKS", Pattern2 = "EVEN_WEEKS"),
    \+ (Pattern1 = "EVEN_WEEKS", Pattern2 = "ODD_WEEKS").

time_minutes(TimeAtom, Minutes) :-
    ensure_string(TimeAtom, TimeStr),
    split_string(TimeStr, ":", "", Parts),
    Parts = [HStr, MStr|_],
    number_string(H, HStr),
    number_string(M, MStr),
    Minutes is H * 60 + M.

meta_to_public_slot(Meta, Public) :-
    Public = _{
        courseId: Meta.courseId,
        roomCode: Meta.roomCode,
        dayOfWeek: Meta.dayOfWeek,
        startTime: Meta.startTime,
        endTime: Meta.endTime,
        validFrom: Meta.validFrom,
        validUntil: Meta.validUntil,
        weekPattern: Meta.weekPattern
    }.

ensure_string(Value, String) :-
    ( string(Value) -> String = Value
    ; atom(Value) -> atom_string(Value, String)
    ; number(Value) -> number_string(Value, String)
    ; term_string(Value, String)
    ).
