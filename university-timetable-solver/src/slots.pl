:- module(slots, [
    build_slot_templates/3,
    slot_index_for_time/6
]).

:- use_module(model).

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
            get_dict(start, S, Start0),
            get_dict(end, S, End0),
            ensure_string(Start0, Start),
            ensure_string(End0, End),
            time_minutes(Start, SM),
            time_minutes(End, EM),
            SM >= EarliestM,
            EM =< LatestM,
            member(Day0, Days),
            ensure_string(Day0, Day)
        ),
        SlotTemplates).

slot_index_for_time(SlotTemplates, Day0, Start0, End0, SlotIdx, Start) :-
    ensure_string(Day0, Day),
    ensure_string(Start0, Start),
    ensure_string(End0, End),
    member(slot_t(Day, Start, End, SlotIdx, _SM, _EM), SlotTemplates),
    !.
