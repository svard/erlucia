%%%-------------------------------------------------------------------
%% @doc erlucia public API
%% @end
%%%-------------------------------------------------------------------

-module(erlucia_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, get_env/1, get_env/2]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    erlucia_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Default) ->
    case application:get_env(erlucia, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.
