-module(erlucia_consumer).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ROUTING_KEY, <<"light-level">>).
-define(EXCHANGE, <<"automation">>).

-record(state, {connection, channel}).

start_link() ->
    lager:info("Starting consumer"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    self() ! connect_amqp,
    {ok, undefined}.

handle_info(connect_amqp, _State) ->
    lager:info("Connecting to message bus"),
    Host = erlucia_app:get_env(rabbit_host),
    Queuename = queue_name("erlucia"),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = Host}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = ?EXCHANGE}),
    #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = Queuename,
                                                                        auto_delete = true}),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{exchange = ?EXCHANGE,
                                                                  queue = Queuename,
                                                                  routing_key = ?ROUTING_KEY}),
    Sub = #'basic.consume'{queue = Queuename, no_ack = true},
    amqp_channel:call(Channel, Sub),
    {noreply, #state{connection = Connection, channel = Channel}};
handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info(#'basic.cancel_ok'{}, _State) ->
    {stop, normal, undefined};
handle_info({#'basic.deliver'{delivery_tag = _Tag}, #amqp_msg{payload = Payload}}, State) ->
    Pid = spawn(erlucia_controller, handle_payload, [Payload]),
    monitor(process, Pid),
    {noreply, State};
handle_info({'DOWN', _Ref, process, _From, Reason}, State) ->
    case Reason of
        normal -> 
            {noreply, State};
        _ ->
            lager:info("erlucia_controller exited unexpectedly ~p", [Reason]),
            erlucia_fsm:stop(),
            {noreply, State}
    end.

terminate(_Reason, #state{connection = Connection, channel = Channel}) ->
    lager:info("Disconnecting from message bus and terminating consumer"),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private
-spec queue_name(iolist()) -> binary().
queue_name(Name) ->
    {ok, Hostname} = inet:gethostname(),
    list_to_binary([Name, <<"@">>, Hostname]).
