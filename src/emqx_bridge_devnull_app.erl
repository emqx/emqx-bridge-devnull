%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_bridge_devnull_app).

-behaviour(application).

-emqx_plugin(bridge).

-export([ start/2
        , stop/1
        , prep_stop/1
        ]).

-define(APP, emqx_bridge_devnull).

start(_Type, _Args) ->
    ClientId = <<"emqx_bridge_devnull">>,
    {ok, Sup} = emqx_bridge_devnull_sup:start_link(),
    ?APP:register_metrics(),
    case ?APP:load(ClientId) of
        {ok, Writers} ->
            emqx_bridge_devnull_cli:load(),
            {ok, Sup, #{client_id => ClientId, wrtiers => Writers}}
    end.

prep_stop(State) ->
    emqx_bridge_devnull:unload(),
    emqx_bridge_devnull_cli:unload(),
    State.

stop(#{writers := Writers}) ->
    lists:foreach(fun(Writer) -> ?APP:stop(Writer) end, Writers).
