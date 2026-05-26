:- module(occupancy, [
    empty_occ/1,
    build_occ_from_assignments/3,
    add_assignment_occ/3,
    candidate_ok/3,
    fixed_conflicts/2,
    hard_conflicts/2,
    put_candidate_occ/4,
    get_mask/3
]).

:- use_module(library(assoc)).
:- use_module(model).

empty_occ(occ(RoomOcc, TeacherOcc, GroupOcc)) :-
    empty_assoc(RoomOcc),
    empty_assoc(TeacherOcc),
    empty_assoc(GroupOcc).

build_occ_from_assignments([], Occ, Occ).
build_occ_from_assignments([A|Rest], Occ0, Occ) :-
    add_assignment_occ(A, Occ0, Occ1),
    build_occ_from_assignments(Rest, Occ1, Occ).

add_assignment_occ(A, occ(RoomOcc0, TeacherOcc0, GroupOcc0), occ(RoomOcc1, TeacherOcc1, GroupOcc1)) :-
    slot_bit(A.slotIndex, Bit),
    put_bit(RoomOcc0, room(A.dayOfWeek, A.roomCode), Bit, RoomOcc1),
    put_bit(TeacherOcc0, teacher(A.dayOfWeek, A.teacherId), Bit, TeacherOcc1),
    put_groups(A.groups, A.dayOfWeek, Bit, GroupOcc0, GroupOcc1).

put_candidate_occ(cand(RoomCode, Day, _Start, _End, SlotIdx),
                  task(_RequestId, _CourseCode, TeacherId, Groups, _ReqCap, _ReqItems),
                  Occ0,
                  Occ1) :-
    A = _{
        roomCode:RoomCode,
        dayOfWeek:Day,
        teacherId:TeacherId,
        groups:Groups,
        slotIndex:SlotIdx
    },
    add_assignment_occ(A, Occ0, Occ1).

candidate_ok(cand(RoomCode, Day, _Start, _End, SlotIdx),
             task(_RequestId, _CourseCode, TeacherId, Groups, _ReqCap, _ReqItems),
             occ(RoomOcc, TeacherOcc, GroupOcc)) :-
    slot_bit(SlotIdx, Bit),
    free_key(RoomOcc, room(Day, RoomCode), Bit),
    free_key(TeacherOcc, teacher(Day, TeacherId), Bit),
    groups_free(Groups, Day, Bit, GroupOcc).

fixed_conflicts(Assignments, Conflicts) :-
    hard_conflicts(Assignments, Conflicts).

hard_conflicts(Assignments, Conflicts) :-
    findall(C,
        (
            append(_, [A|Rest], Assignments),
            member(B, Rest),
            assignment_conflict(A, B, C)
        ),
        Conflicts).

assignment_conflict(A, B, Conflict) :-
    same_day_overlap(A, B),
    A.roomCode = B.roomCode,
    assignment_public_id(A, AId),
    assignment_public_id(B, BId),
    Conflict = _{
        type:"ROOM_CONFLICT",
        slotIds:[AId, BId],
        roomCode:A.roomCode,
        dayOfWeek:A.dayOfWeek,
        startTime:A.startTime,
        endTime:A.endTime
    }.
assignment_conflict(A, B, Conflict) :-
    same_day_overlap(A, B),
    A.teacherId = B.teacherId,
    assignment_public_id(A, AId),
    assignment_public_id(B, BId),
    Conflict = _{
        type:"TEACHER_CONFLICT",
        slotIds:[AId, BId],
        teacherId:A.teacherId,
        dayOfWeek:A.dayOfWeek,
        startTime:A.startTime,
        endTime:A.endTime
    }.
assignment_conflict(A, B, Conflict) :-
    same_day_overlap(A, B),
    member(GA, A.groups),
    member(GB, B.groups),
    ensure_string(GA, G),
    ensure_string(GB, G),
    assignment_public_id(A, AId),
    assignment_public_id(B, BId),
    Conflict = _{
        type:"GROUP_CONFLICT",
        slotIds:[AId, BId],
        groupId:G,
        dayOfWeek:A.dayOfWeek,
        startTime:A.startTime,
        endTime:A.endTime
    }.

same_day_overlap(A, B) :-
    A.dayOfWeek = B.dayOfWeek,
    times_overlap(A.startTime, A.endTime, B.startTime, B.endTime).

assignment_public_id(A, Id) :-
    (   get_dict(slotId, A, V)
    ->  ensure_string(V, Id)
    ;   get_dict(requestId, A, V)
    ->  ensure_string(V, Id)
    ;   get_dict(courseCode, A, V)
    ->  ensure_string(V, Id)
    ;   Id = "unknown"
    ).

put_groups([], _Day, _Bit, G0, G0).
put_groups([G|Gs], Day, Bit, G0, G2) :-
    ensure_string(G, GS),
    put_bit(G0, group(Day, GS), Bit, G1),
    put_groups(Gs, Day, Bit, G1, G2).

groups_free([], _Day, _Bit, _GroupOcc).
groups_free([G|Gs], Day, Bit, GroupOcc) :-
    ensure_string(G, GS),
    free_key(GroupOcc, group(Day, GS), Bit),
    groups_free(Gs, Day, Bit, GroupOcc).

get_mask(Assoc, Key, Mask) :-
    (   get_assoc(Key, Assoc, Mask0)
    ->  Mask = Mask0
    ;   Mask = 0
    ).

free_key(Assoc, Key, Bit) :-
    get_mask(Assoc, Key, Mask),
    Mask /\ Bit =:= 0.

put_bit(Assoc0, Key, Bit, Assoc) :-
    get_mask(Assoc0, Key, Mask0),
    Mask is Mask0 \/ Bit,
    put_assoc(Key, Assoc0, Mask, Assoc).
