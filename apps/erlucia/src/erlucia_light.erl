-module(erlucia_light).

-export([switch_on/1, switch_off/1]).

-spec switch_on(integer() | list()) -> 'ok' | {'error', string()}.
switch_on(Ids) when is_list(Ids) ->
    lists:foreach(fun(Id) -> switch_on(Id) end, Ids);
switch_on(Id) ->
    Body = jsx:encode(#{<<"state">> => <<"ON">>}),
    send(Id, Body).

-spec switch_off(integer() | list()) -> 'ok' | {'error', string()}.
switch_off(Ids) when is_list(Ids) ->
     lists:foreach(fun(Id) -> switch_off(Id) end, Ids);
switch_off(Id) ->
    Body = jsx:encode(#{<<"state">> => <<"OFF">>}),
    send(Id, Body).

%% Private
-spec send(integer(), binary()) -> 'ok' | {'error', string()}.
send(Id, Body) ->
    Url = erlucia_app:get_env(light_api_url),
    Path = Url ++ integer_to_list(Id),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],

    case hackney:request(put, list_to_binary(Path), Headers, Body, []) of
        {ok, 201, _RespHeaders, _ClientRef} ->
            lager:info("Light switch successful, id ~p", [Id]),
            ok;
        
        Resp ->
            lager:error("Request to ~p failed with ~p", [Url, Resp]),
            {error, Resp}
    end.
