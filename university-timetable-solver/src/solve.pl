:- module(solve, [solve_request/2]).

:- use_module(validate).
:- use_module(policy).
:- use_module(model).
:- use_module(candidates).
:- use_module(search).
:- use_module(scoring).

solve_request(Body, Response) :-
    ensure_required(Body),

    get_dict(semester, Body, Semester),
    get_dict(courses, Body, Courses0),
    get_dict(rooms, Body, Rooms),
    get_dict(teachers, Body, Teachers),
    get_dict(policy, Body, PolicyIn),

    instant_date(Semester.startAt, StartDate),
    instant_date(Semester.endAt, EndDate),

    parse_policy(PolicyIn, Policy),

    build_slot_templates(Policy.grid, Policy.limits, SlotTemplates),

    expand_courses_by_hours(Courses0, ExpandedCourses),
    build_tasks(ExpandedCourses, Tasks),

    build_all_candidates(Tasks, Rooms, Teachers, SlotTemplates, Policy, TasksWithCands0),
    sort_tasks_mrv(TasksWithCands0, TasksWithCands),

    search_best(TasksWithCands, Policy, BestAssignments, BestUnplaced, BasePenalty),

    soft_penalty(BestAssignments, Policy, SoftPenalty),
    TotalPenalty is BasePenalty + SoftPenalty,
    score_from_penalty(TotalPenalty, Score),

    public_slots(BestAssignments, StartDate, EndDate, PublicSlots),
    public_unplaced(BestUnplaced, PublicUnplaced),

    Response = _{
        slots: PublicSlots,
        unplaced: PublicUnplaced,
        evaluationPenalty: TotalPenalty,
        evaluationScore: Score
    }.

build_slot_templates(Grid, Limits, SlotTemplates) :-
    get_dict(days, Grid, Days),
    get_dict(slots, Grid, TimeSlots),

    get_dict(timeBounds, Limits, TB),
    get_dict(earliestStart, TB, Earliest),
    get_dict(latestEnd, TB, Latest),
    time_minutes(Earliest, EarliestM),
    time_minutes(Latest, LatestM),

    findall(slot_t(Day, Start, End, SlotIdx, SM, EM),
        (
            nth1(SlotIdx, TimeSlots, S),
            get_dict(start, S, Start),
            get_dict(end, S, End),
            time_minutes(Start, SM),
            time_minutes(End, EM),
            SM >= EarliestM,
            EM =< LatestM,
            member(Day, Days)
        ),
        SlotTemplates).

public_slots(Assignments, StartDate, EndDate, PublicSlots) :-
    findall(Slot,
        (
            member(A, Assignments),
            Slot = _{
                courseId: A.courseId,
                roomCode: A.roomCode,
                dayOfWeek: A.dayOfWeek,
                startTime: A.startTime,
                endTime: A.endTime,
                validFrom: StartDate,
                validUntil: EndDate,
                weekPattern: "EVERY_WEEK"
            }
        ),
        PublicSlots).

public_unplaced(Unplaced, PublicUnplaced) :-
    findall(U,
        (
            member(unplaced_item(CourseId, Reason), Unplaced),
            U = _{courseId: CourseId, reason: Reason}
        ),
        PublicUnplaced).
