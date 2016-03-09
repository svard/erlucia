-module(erlucia_controller).

-export([check/1]).

check(#{<<"date">> := Date, <<"level">> := Level}) when Level < 0.1 ->
    Datetime = iso8601:parse(Date),
    {_, {Hour, _, _}} = calendar:universal_time_to_local_time(Datetime),
    lager:info("Level is under threshold: ~p", [Level]),

    if 
        Hour < 23 -> erlucia_fsm:next();
        Hour == 23 -> ignore
    end;

check(#{<<"date">> := Date, <<"level">> := Level}) ->
    Datetime = iso8601:parse(Date),
    {_, {Hour, _, _}} = calendar:universal_time_to_local_time(Datetime),
    State = erlucia_fsm:get(),
    lager:info("Level is above threshold: ~p", [Level]),

    case {State, Hour} of
        {true, 11} -> erlucia_fsm:reset();
        {true, _} -> ignore;
        {false, _} -> erlucia_fsm:reset()
    end.
