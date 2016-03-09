-module(erlucia_consumer).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0]).

-export([loop/0]).

-define(ROUTING_KEY, <<"light-level">>).
-define(EXCHANGE, <<"automation">>).

start_link() ->
    lager:info("Starting consumer"),
    {ok, Host} = application:get_env(rabbit_host),
    Queuename = queue_name("erlucia"),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = Host}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = ?EXCHANGE}),

    #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = Queuename,
                                                                        auto_delete = true}),

    #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{exchange = ?EXCHANGE,
                                                                  queue = Queuename,
                                                                  routing_key = ?ROUTING_KEY}),

    Pid = spawn_link(?MODULE, loop, []),
    Sub = #'basic.consume'{queue = Queuename, no_ack = true},
    amqp_channel:subscribe(Channel, Sub, Pid),
    {ok, Pid}.

loop() ->
    receive
        #'basic.consume_ok'{} ->
            loop();
        
        #'basic.cancel_ok'{} ->
            ok;

        {#'basic.deliver'{delivery_tag = _Tag}, #amqp_msg{payload = Payload}} ->
            Json = jsx:decode(Payload, [return_maps]),
            spawn(erlucia_controller, check, [Json]),
            loop()
    end.

queue_name(Name) ->
    {ok, Hostname} = inet:gethostname(),
    list_to_binary(Name ++ "@" ++ Hostname).
