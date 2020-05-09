%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_bridge_devnull).

-include_lib("emqx/include/emqx.hrl").

%% Callbacks of ecpool Worker
-export([ register_metrics/0
        , load/1
        , unload/0
        , stop/1
        ]).

%% Hook Callbacks
-export([ on_client_connected/3
        , on_client_disconnected/4
        , on_session_subscribed/4
        , on_session_unsubscribed/4
        , on_message_publish/2
        , on_message_delivered/3
        , on_message_acked/3
        ]).

-record(config, {filter,
                 payload_format
                }).

-record(writer, {filter,
                 payload_format,
                 encoder,
                 fd
                }).

register_metrics() ->
    [emqx_metrics:new(MetricName) || MetricName <- ['bridge.devnull.client_connected',
                                                    'bridge.devnull.client_disconnected',
                                                    'bridge.devnull.session_subscribed',
                                                    'bridge.devnull.session_unsubscribed',
                                                    'bridge.devnull.message_publish',
                                                    'bridge.devnull.message_acked',
                                                    'bridge.devnull.message_delivered']].

-define(APP, emqx_bridge_devnull).

%%--------------------------------------------------------------------
%% Load The Plugin
%%--------------------------------------------------------------------
load(_ClientId) ->
    HookList = parse_hook(application:get_env(?APP, hooks, [])),
    Writers =
        lists:foldl(fun({Hook, Config}, Acc) ->
                        {ok, F} = file:open("/dev/null", [write, binary]),
                        Writer = #writer{filter = Config#config.filter,
                                         payload_format = Config#config.payload_format,
                                         encoder = gen_encoder(Hook),
                                         fd = F
                                        },
                        [hook(Hook, Writer) | Acc]
                    end, [], HookList),
    io:format("emqx_bridge_devnull is loaded.~n"),
    {ok, Writers}.

hook(Hook, Params) ->
    case Hook of
        'client.connected'     -> emqx:hook(Hook, fun ?MODULE:on_client_connected/3, [Params]);
        'client.disconnected'  -> emqx:hook(Hook, fun ?MODULE:on_client_disconnected/4, [Params]);
        'session.subscribed'   -> emqx:hook(Hook, fun ?MODULE:on_session_subscribed/4, [Params]);
        'session.unsubscribed' -> emqx:hook(Hook, fun ?MODULE:on_session_unsubscribed/4, [Params]);
        'message.publish'      -> emqx:hook(Hook, fun ?MODULE:on_message_publish/2, [Params]);
        'message.acked'        -> emqx:hook(Hook, fun ?MODULE:on_message_acked/3, [Params]);
        'message.delivered'    -> emqx:hook(Hook, fun ?MODULE:on_message_delivered/3, [Params])
    end.

%%--------------------------------------------------------------------
%% Unload the Plugin
%%--------------------------------------------------------------------
unload() ->
    HookList = parse_hook(application:get_env(?APP, hooks, [])),
    lists:foreach(fun({Hook, _Config}) ->
        unload_(Hook)
    end, HookList),
    io:format("~s is unloaded.~n", [?APP]), ok.

unload_(Hook) ->
    case Hook of
        'client.connected'     -> emqx:unhook(Hook, fun ?MODULE:on_client_connected/3);
        'client.disconnected'  -> emqx:unhook(Hook, fun ?MODULE:on_client_disconnected/4);
        'session.subscribed'   -> emqx:unhook(Hook, fun ?MODULE:on_session_subscribed/4);
        'session.unsubscribed' -> emqx:unhook(Hook, fun ?MODULE:on_session_unsubscribed/3);
        'message.publish'      -> emqx:unhook(Hook, fun ?MODULE:on_message_publish/2);
        'message.acked'        -> emqx:unhook(Hook, fun ?MODULE:on_message_acked/3);
        'message.delivered'    -> emqx:unhook(Hook, fun ?MODULE:on_message_delivered/3)
    end.

stop(#writer{fd = Fd}) -> file:close(Fd).

%%--------------------------------------------------------------------
%% Client connected
%%--------------------------------------------------------------------
on_client_connected(ClientInfo, _ConnInfo, Writer) ->
    emqx_metrics:inc('bridge.devnull.client_connected'),
    ClientId = maps:get(clientid, ClientInfo, undefined),
    Username = maps:get(username, ClientInfo, undefined),
    Data = [{clientid, ClientId},
            {username, Username},
            {node, a2b(node())},
            {ts, erlang:system_time(millisecond)}],
    write(?FUNCTION_NAME, Writer, [ClientId, Username], Data),
    ok.

%%--------------------------------------------------------------------
%% Client disconnected
%%--------------------------------------------------------------------
on_client_disconnected(ClientInfo, {shutdown, Reason}, ConnInfo, Writer)
  when is_atom(Reason); is_integer(Reason) ->
    on_client_disconnected(ClientInfo, Reason, ConnInfo, Writer);
on_client_disconnected(ClientInfo, Reason, _ConnInfo, Writer)
  when is_atom(Reason); is_integer(Reason) ->
    emqx_metrics:inc('bridge.devnull.client_disconnected'),
    ClientId = maps:get(clientid, ClientInfo, undefined),
    Username = maps:get(username, ClientInfo, undefined),
    Data = [{clientid, ClientId},
            {username, Username},
            {node, a2b(node())},
            {reason, a2b(Reason)},
            {ts, erlang:system_time(millisecond)}],
    write(?FUNCTION_NAME, Writer, [ClientId, Username], Data),
    ok;
on_client_disconnected(_ClientInfo, Reason, _ConnInfo, _Writer) ->
    logger:error("Client disconnected reason:~p not encode json", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Session subscribed
%%--------------------------------------------------------------------
on_session_subscribed(ClientInfo, Topic, Opts,
                      #writer{filter = Filter} = Writer) ->
    case emqx_topic:match(Topic, Filter) of
        true  ->
            ClientId = maps:get(clientid, ClientInfo, undefined),
            Username = maps:get(username, ClientInfo, undefined),
            emqx_metrics:inc('bridge.devnull.session_subscribed'),
            Data = format_sub_json(ClientId, Topic, Opts),
            write(?FUNCTION_NAME, Writer, [ClientId, Username, Topic], Data);
        false -> ok
    end,
    ok.

%%--------------------------------------------------------------------
%% Session unsubscribed
%%--------------------------------------------------------------------
on_session_unsubscribed(ClientInfo, Topic, Opts,
                        #writer{filter = Filter} = Writer) ->
    case emqx_topic:match(Topic, Filter) of
        true  ->
            ClientId = maps:get(clientid, ClientInfo, undefined),
            Username = maps:get(username, ClientInfo, undefined),
            emqx_metrics:inc('bridge.devnull.session_unsubscribed'),
            Data = format_sub_json(ClientId, Topic, Opts),
            write(?FUNCTION_NAME, Writer, [ClientId, Username, Topic], Data);
        false -> ok
    end,
    ok.

%%--------------------------------------------------------------------
%% Publish message
%%--------------------------------------------------------------------
on_message_publish(Msg = #message{topic = Topic, from = From, headers = Headers},
                   #writer{filter = Filter,
                           payload_format = PayloadFormat} = Writer)->
    case emqx_topic:match(Topic, Filter) of
        true ->
            emqx_metrics:inc('bridge.devnull.message_publish'),
            Data = format_pub_msg(Msg, PayloadFormat),
            Username = maps:get(username, Headers, undefined),
            write(?FUNCTION_NAME, Writer, [From, Username, Topic], Data);
        false -> ok
    end,
    {ok, Msg}.

%%--------------------------------------------------------------------
%% Revc message acked
%%--------------------------------------------------------------------
on_message_acked(ClientInfo, Msg = #message{topic = Topic},
                 #writer{filter = Filter,
                         payload_format = PayloadFormat
                         } = Writer) ->
    case emqx_topic:match(Topic, Filter) of
        true ->
            emqx_metrics:inc('bridge.devnull.message_acked'),
            ClientId = maps:get(clientid, ClientInfo, undefined),
            Username = maps:get(username, ClientInfo, undefined),
            Data = format_revc_msg(ClientId, Username, Msg, PayloadFormat),
            write(?FUNCTION_NAME, Writer, [ClientId, Username, Topic], Data);
        false -> ok
    end,
    {ok, Msg}.

%%--------------------------------------------------------------------
%% Revc message delivered
%%--------------------------------------------------------------------
on_message_delivered(ClientInfo, Msg = #message{topic = Topic},
                     #writer{filter = Filter,
                             payload_format = PayloadFormat} = Writer) ->
    case emqx_topic:match(Topic, Filter) of
        true ->
            emqx_metrics:inc('bridge.devnull.message_delivered'),
            ClientId = maps:get(clientid, ClientInfo, undefined),
            Username = maps:get(username, ClientInfo, undefined),
            Data = format_revc_msg(ClientId, Username, Msg, PayloadFormat),
            write(?FUNCTION_NAME, Writer, [ClientId, Username, Topic], Data);
        false -> ok
    end,
    {ok, Msg}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
parse_hook(Hooks) ->
    parse_hook(Hooks, [], 0).

parse_hook([], Acc, _Seq) ->
    Acc;
parse_hook([{Hook, Item} | Hooks], Acc, Seq) ->
    NewSeq = Seq+1,
    Params = emqx_json:decode(Item),
    Filter = proplists:get_value(<<"filter">>, Params),
    PayloadFormat = a2b(application:get_env(?APP, encode_payload_type, base64)),
    Config = #config{filter = Filter,
                     payload_format = PayloadFormat
                    },
    parse_hook(Hooks, [{l2a(Hook), Config} | Acc], NewSeq).

format_sub_json(ClientId, Topic, Opts) ->
    Qos = maps:get(qos, Opts, 0),
    [{clientid, ClientId},
     {topic, Topic},
     {qos, Qos},
     {node, a2b(node())},
     {ts, erlang:system_time(millisecond)}].

payload_format(Payload, PayloadFormat) ->
    case PayloadFormat of
    <<"base64">> ->
        base64:encode(Payload);
    _ ->
        Payload
    end.

format_pub_msg(Msg, PayloadFormat) ->
    #message{from = From,
             topic = Topic,
             payload = Payload,
             headers = Headers,
             qos = Qos,
             timestamp = Ts} = Msg,
    Username = maps:get(username, Headers, undefined),
    [{clientid, From},
     {username, Username},
     {topic, Topic},
     {payload, payload_format(Payload, PayloadFormat)},
     {qos, Qos},
     {node, a2b(node())},
     {ts, Ts}].

format_revc_msg(ClientId, Username, Msg, PayloadFormat) ->
    #message{from = From,
             topic = Topic,
             payload = Payload,
             qos = Qos,
             timestamp = Ts} = Msg,
    [{clientid, ClientId},
     {username, Username},
     {from, From},
     {topic, Topic},
     {payload, payload_format(Payload, PayloadFormat)},
     {qos, Qos},
     {node, a2b(node())},
     {ts, Ts}].

gen_encoder('client.connected') ->
    {ok, SchemaJSON} = file:read_file(code:priv_dir(?MODULE)++"/connect.avsc"),
    avro:make_simple_encoder(jsx:format(SchemaJSON), []);
gen_encoder('client.disconnected') ->
    {ok, SchemaJSON} = file:read_file(code:priv_dir(?MODULE)++"/disconnect.avsc"),
    avro:make_simple_encoder(jsx:format(SchemaJSON), []);
gen_encoder('session.subscribed') ->
    {ok, SchemaJSON} = file:read_file(code:priv_dir(?MODULE)++"/subscribe.avsc"),
    avro:make_simple_encoder(jsx:format(SchemaJSON), []);
gen_encoder('session.unsubscribed') ->
    {ok, SchemaJSON} = file:read_file(code:priv_dir(?MODULE)++"/subscribe.avsc"),
    avro:make_simple_encoder(jsx:format(SchemaJSON), []);
gen_encoder('message.publish') ->
    {ok, SchemaJSON} = file:read_file(code:priv_dir(?MODULE)++"/publish.avsc"),
    avro:make_simple_encoder(jsx:format(SchemaJSON), []);
gen_encoder('message.acked') ->
    {ok, SchemaJSON} = file:read_file(code:priv_dir(?MODULE)++"/receive.avsc"),
    avro:make_simple_encoder(jsx:format(SchemaJSON), []);
gen_encoder('message.delivered') ->
    {ok, SchemaJSON} = file:read_file(code:priv_dir(?MODULE)++"/receive.avsc"),
    avro:make_simple_encoder(jsx:format(SchemaJSON), []).

l2a(L) -> erlang:list_to_atom(L).
a2b(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
a2b(A) -> A.

write(FunctionName, #writer{fd = Fd, encoder = Encoder}, Key, Data0) ->
  %% erlavro takes undefined as not-present
  F = fun({K, undefined}) -> {K, <<>>};
         (X) -> X
      end,
  Data = lists:map(F, Data0),
  Encoded = [Key, Encoder(Data)],
  ok = emqx_bridge_devnull_writer:write(FunctionName, Fd, Encoded).

