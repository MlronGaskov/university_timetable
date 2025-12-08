:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).

:- http_handler(root(solve), solve_handler, []).

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

solve_handler(Request) :-
    http_read_json_dict(Request, _Body, []),

    MockResponse = _{
        slots: [
            _{
                courseId: "00000000-0000-0000-0000-000000000001",
                roomCode: "MAIN-101",
                dayOfWeek: "MONDAY",
                startTime: "09:00",
                endTime: "10:30",
                validFrom: "2025-02-10",
                validUntil: "2025-06-01",
                weekPattern: "ODD_WEEKS"
            },
            _{
                courseId: "00000000-0000-0000-0000-000000000002",
                roomCode: "MAIN-202",
                dayOfWeek: "MONDAY",
                startTime: "10:40",
                endTime: "12:10",
                validFrom: "2025-02-10",
                validUntil: "2025-04-01",
                weekPattern: "EVERY_WEEK"
            }
        ]
    },

    http_reply_json_dict(MockResponse).
