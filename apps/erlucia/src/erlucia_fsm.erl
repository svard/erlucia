-module(erlucia_fsm).

-behaviour(gen_fsm).

-export([start_link/0]).
%% Callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
%% States
-export([check_one/2, check_two/2, switch/2]).
%% API
-export([next/0, reset/0, get/0]).

start_link() ->
    lager:info("Starting FSM"),
    gen_fsm:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, check_one, #{triggered => false}}.

check_one(next, Data) ->
    {next_state, check_two, Data}.

check_two(next, Data) ->
    {next_state, switch, Data}.

switch(next, #{triggered := false}) ->
    {ok, Ids} = application:get_env(light_ids),
    lists:foreach(fun(Id) -> erlucia_light:switch_on(Id) end, Ids),
    {next_state, switch, #{triggered => true}};

switch(next, Data) ->
    {next_state, switch, Data}.

%% Callbacks
handle_event(reset, _State_name, _Data) ->
    {next_state, check_one, #{triggered => false}}.

handle_sync_event(get, _From, State, Data) ->
    {reply, maps:get(triggered, Data), State, Data}.

handle_info(_Info, State, Data) ->
    {next_state, State, Data}.

terminate(normal, _State, _Data) ->
    ok;
terminate(Reason, _State, _Data) ->
    lager:error("FSM terminated with reason ~p", [Reason]),
    ok.

code_change(_Old_version, State, Data, _Extra) ->
    {ok, State, Data}.

%% API
next() ->
    gen_fsm:send_event({global, ?MODULE}, next).

reset() ->
    gen_fsm:send_all_state_event({global, ?MODULE}, reset).

get() ->
    gen_fsm:sync_send_all_state_event({global, ?MODULE}, get).
