:- module(policy, [parse_policy/2]).

:- use_module(library(http/json)).
:- use_module(model).

parse_policy(PolicyIn, PolicyOut) :-
    get_dict_default(gridJson, PolicyIn, "", GridRaw),
    get_dict_default(breaksJson, PolicyIn, "{}", BreaksRaw),
    get_dict_default(limitsJson, PolicyIn, "{}", LimitsRaw),
    get_dict_default(weightsJson, PolicyIn, "{}", WeightsRaw),
    get_dict_default(searchJson, PolicyIn, "{}", SearchRaw),

    parse_grid(GridRaw, Grid),
    parse_breaks(BreaksRaw, Breaks),
    parse_limits(LimitsRaw, Limits),
    parse_weights(WeightsRaw, Weights),
    parse_search(SearchRaw, Search),

    PolicyOut = _{
        grid: Grid,
        breaks: Breaks,
        limits: Limits,
        weights: Weights,
        search: Search
    }.

get_dict_default(Key, Dict, Default, Value) :-
    ( get_dict(Key, Dict, V) -> Value = V ; Value = Default ).

parse_grid(Raw, Grid) :-
    default_grid(Default),
    parse_json_to_dict(Raw, Parsed, Default),
    get_dict_default(days, Parsed, Default.days, Days),
    get_dict_default(slots, Parsed, Default.slots, Slots),
    Grid = _{ version: 1, days: Days, slots: Slots }.

default_grid(_{
    version: 1,
    days: ["MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY"],
    slots: [
        _{start:"09:00", end:"10:35"},
        _{start:"10:50", end:"12:25"},
        _{start:"12:40", end:"14:15"},
        _{start:"14:30", end:"16:05"},
        _{start:"16:20", end:"17:55"},
        _{start:"18:10", end:"19:45"}
    ]
}).

parse_breaks(Raw, Breaks) :-
    default_breaks(Default),
    parse_json_to_dict(Raw, Parsed, Default),
    get_dict_default(lunch, Parsed, Default.lunch, Lunch),
    Breaks = _{version:1, lunch:Lunch}.

default_breaks(_{
    version: 1,
    lunch: _{enabled:false, windowStart:"12:00", windowEnd:"15:00", minFreeMinutes:30}
}).

parse_limits(Raw, Limits) :-
    default_limits(Default),
    parse_json_to_dict(Raw, Parsed, Default),
    get_dict_default(maxPairsPerDay, Parsed, Default.maxPairsPerDay, MPD),
    get_dict_default(maxConsecutivePairs, Parsed, Default.maxConsecutivePairs, MCP),
    get_dict_default(timeBounds, Parsed, Default.timeBounds, TB),
    Limits = _{version:1, maxPairsPerDay:MPD, maxConsecutivePairs:MCP, timeBounds:TB}.

default_limits(_{
    version: 1,
    maxPairsPerDay: _{teacher: 6, group: 6},
    maxConsecutivePairs: _{teacher: 3, group: 4},
    timeBounds: _{earliestStart:"08:00", latestEnd:"22:00"}
}).

parse_weights(Raw, Weights) :-
    default_weights(Default),
    parse_json_to_dict(Raw, Parsed, Default),
    get_dict_default(hard, Parsed, Default.hard, Hard),
    get_dict_default(soft, Parsed, Default.soft, Soft),
    Weights = _{version:1, hard:Hard, soft:Soft}.

default_weights(_{
    version: 1,
    hard: _{ unplacedPair: 1000 },
    soft: _{
        missingLunch: 3,
        tooManyPairsPerDay: 2,
        tooManyConsecutive: 1
    }
}).

parse_search(Raw, Search) :-
    default_search(Default),
    parse_json_to_dict(Raw, Parsed, Default),
    get_dict_default(timeLimitMs, Parsed, Default.timeLimitMs, TL),
    get_dict_default(restarts, Parsed, Default.restarts, R),
    get_dict_default(topK, Parsed, Default.topK, K),
    get_dict_default(maxRoomsPerTask, Parsed, Default.maxRoomsPerTask, MR),
    Search = _{version:1, timeLimitMs:TL, restarts:R, topK:K, maxRoomsPerTask:MR}.

default_search(_{
    version: 1,
    timeLimitMs: 5000,
    restarts: 100000000,
    topK: 20,
    maxRoomsPerTask: 20
}).

parse_json_to_dict(Raw, DictOut, Default) :-
    ( is_dict(Raw) -> DictOut = Raw
    ; is_list(Raw) -> DictOut = Raw
    ; ensure_string(Raw, S),
      ( S = "" -> DictOut = Default
      ; atom_string(A, S),
        ( catch(atom_json_dict(A, D, []), _, fail)
        -> DictOut = D
        ;  DictOut = Default
        )
      )
    ).
