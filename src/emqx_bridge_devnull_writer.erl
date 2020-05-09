%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

%% this module wraps around file:write for a clean mock(meck) in common test.
-module(emqx_bridge_devnull_writer).

-include_lib("emqx/include/emqx.hrl").

-export([write/3]).

write(_HookName, Fd, Data) ->
  file:write(Fd, Data).
