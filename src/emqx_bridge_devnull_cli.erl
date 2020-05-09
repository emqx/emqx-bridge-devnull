%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module (emqx_bridge_devnull_cli).

-export([ load/0
        , devnull_stats/1
        , unload/0
        ]).


-define(PRINT(Format, Args), io:format(Format, Args)).

-define(PRINT_CMD(Cmd, Descr), io:format("~-48s# ~s~n", [Cmd, Descr])).

-define(USAGE(CmdList), [?PRINT_CMD(Cmd, Descr) || {Cmd, Descr} <- CmdList]).


load() ->
    emqx_ctl:register_command(devnull_stats, {?MODULE, devnull_stats}, []).

devnull_stats([]) ->
    lists:foreach(
        fun({Stat, Val}) ->
            ?PRINT("~-20s: ~w~n", [Stat, Val])
        end, maps:to_list(wolff_stats:getstat()));

devnull_stats(_) ->
    ?USAGE([{"devnull_stats",  "Bridge devnull message stats"}]).

unload() ->
    emqx_ctl:unregister_command(devnull_stats).
