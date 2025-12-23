:- module(server, [start_server/1]).

:- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

:- use_module('src/solve.pl').

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
