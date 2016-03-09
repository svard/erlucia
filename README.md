erlucia
=====

Same application as lucia but written in good old Erlang/OTP instead of Elixir.

The purpose of the application is to receive data from a light sensor over a RabbitMQ message bus and decide when a configured number of lights should be switched on.

Build
-----

    $ rebar3 compile
