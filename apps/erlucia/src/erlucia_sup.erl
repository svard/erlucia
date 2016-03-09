%%%-------------------------------------------------------------------
%% @doc erlucia top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlucia_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Children = [{erlucia_consumer, {erlucia_consumer, start_link, []}, transient, 2000, worker, [erlucia_consumer]},
                {erlucia_fsm, {erlucia_fsm, start_link, []}, permanent, 2000, worker, [erlucia_fsm]}],
    {ok, { {one_for_one, 0, 1}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
