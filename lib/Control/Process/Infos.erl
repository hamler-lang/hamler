%%---------------------------------------------------------------------------
%% |
%% Module      :  Infos
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Process Infos FFI module.
%%
%%---------------------------------------------------------------------------
-module('Infos').

-include("../../Foreign.hrl").

-export([processInfo/1, processMsgs/1]).

processInfo(Pid) ->
  ?IO(erlang:process_info(Pid)).

processMsgs(Pid) ->
  ?IO(element(2, erlang:process_info(Pid, messages))).
