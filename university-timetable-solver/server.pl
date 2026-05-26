:- module(server, [start_server/1]).

:- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- use_module('src/solve.pl').

:- http_handler(root(solve), solve_handler, []).
:- http_handler(root(health), health_handler, []).

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

health_handler(_Request) :-
    reply_json_dict(_{status:"ok"}).

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
    reply_json_dict(_{error:"INVALID_REQUEST", message:Message}, [status(400)]).
reply_error(error(fixed_slots_conflict(Conflicts), _)) :-
    !,
    reply_json_dict(_{error:"FIXED_SLOTS_CONFLICT", conflicts:Conflicts}, [status(400)]).
reply_error(error(generated_slots_conflict(Conflicts), _)) :-
    !,
    reply_json_dict(_{error:"GENERATED_SLOTS_CONFLICT", conflicts:Conflicts}, [status(500)]).
reply_error(Error) :-
    message_to_string(Error, Msg),
    reply_json_dict(_{error:"INTERNAL_ERROR", message:Msg}, [status(500)]).
