-module(erlucia_fsm).

-behaviour(gen_fsm).

-export([start_link/0]).
%% Callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
%% States
-export([check_one/2, check_two/2, switch/2, idle/2]).
%% API
-export([next/0, reset/0, get/0, stop/0]).

-record(state, {triggered}).

start_link() ->
    lager:info("Starting FSM"),
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, check_one, #state{triggered = false}}.

check_one(next, Data) ->
    {next_state, check_two, Data}.

check_two(next, Data) ->
    {next_state, switch, Data}.

switch(next, #state{triggered = false}) ->
    Ids = erlucia_app:get_env(light_ids, []),
    spawn_link(erlucia_light, switch_on, [Ids]),
    {next_state, idle, #state{triggered = true}};
switch(next, Data) ->
    {next_state, idle, Data}.

idle(next, Data) ->
    {next_state, idle, Data}.

%% Callbacks
handle_event(reset, _State, _Data) ->
    {next_state, check_one, #state{triggered = false}}.

handle_sync_event(get, _From, State, #state{triggered = Triggered} = Data) ->
    {reply, Triggered, State, Data}.

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
-spec next() -> 'ok'.
next() ->
    gen_fsm:send_event(?MODULE, next).

-spec reset() -> 'ok'.
reset() ->
    gen_fsm:send_all_state_event(?MODULE, reset).

-spec get() -> boolean().
get() ->
    gen_fsm:sync_send_all_state_event(?MODULE, get).

-spec stop() -> 'ok'.
stop() ->
    gen_fsm:stop(?MODULE).
