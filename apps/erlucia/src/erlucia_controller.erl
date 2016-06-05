-module(erlucia_controller).

-export([handle_payload/1]).

handle_payload(Payload) ->
    Json = jsx:decode(Payload, [return_maps]),
    check(Json).

check(#{<<"date">> := Date, <<"level">> := Level}) when Level < 0.1 ->
    Datetime = iso8601:parse(Date),
    {_, {Hour, _, _}} = calendar:universal_time_to_local_time(Datetime),
    lager:info("Level is under threshold: ~p", [Level]),
    step_fsm(Hour);
check(#{<<"date">> := Date, <<"level">> := Level}) ->
    lager:info("Level is above threshold: ~p", [Level]),
    erlucia_fsm:reset().

step_fsm(Hour) when (Hour > 12) and (Hour < 23) ->
    erlucia_fsm:next();
step_fsm(_Hour) ->
    ignored.
