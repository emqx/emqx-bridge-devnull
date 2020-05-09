-module(emqx_bridge_devnull_SUITE).

-compile(export_all).

-include_lib("emqx/include/emqx.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-record(callback, {tag,
                   function,
                   init_args,
                   priority  = 0}).

-define(WAIT(PATTERN),
        fun() ->
          receive
            PATTERN ->
              ct:log("PATTERN:~p~n", [PATTERN]),
              ok
          after 5000 ->
            ct:fail({timeout, PATTERN})
          end
        end()).


all() ->
    [{group, emqx_bridge_devnull}].

groups() ->
    [{emqx_bridge_devnull, [sequence],
     [hook_events,
      hook_reset]}].

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx, emqx_bridge_devnull]),
    Config.

end_per_suite(_Config) ->
    emqx_ct_helpers:stop_apps([emqx_bridge_devnull, emqx]).

%% TEST CASES
hook_events(_Config) ->
    meck:new(emqx_bridge_devnull_writer, []),
    Self = self(),
    meck:expect(emqx_bridge_devnull_writer, write, fun(Hook, _Fd, _Data) -> Self ! Hook, ok end),
    {ok, T1} = emqtt:start_link([{host, "localhost"},
                                 {clientid, <<"dummyclient">>}]),

    {ok, _} = emqtt:connect(T1),
    ?WAIT(on_client_connected),

    {ok, _, [1]} = emqtt:subscribe(T1, <<"topic1">>, 1),
    ?WAIT(on_session_subscribed),

    emqtt:publish(T1, <<"topic1">>, <<"hi">>, 1),
    ?WAIT(on_message_publish),

    ?WAIT(on_message_delivered),

    ?WAIT(on_message_acked),

    emqtt:unsubscribe(T1, <<"topic1">>),
    ?WAIT(on_session_unsubscribed),

    emqtt:disconnect(T1),
    ?WAIT(on_client_disconnected),
    true = meck:validate(emqx_bridge_devnull_writer),
    meck:unload(emqx_bridge_devnull_writer),
    ok.

hook_reset(_Config) ->
    Before = emqx_hooks:lookup('client.connected'),
    ok = application:stop(emqx_bridge_devnull),
    After = emqx_hooks:lookup('client.connected'),
    ?assertEqual(length(Before)-1, length(After)),
    ok.
