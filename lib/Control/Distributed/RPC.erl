%%---------------------------------------------------------------------------
%% |
%% Module      :  RPC
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The RPC FFI module.
%%
%%---------------------------------------------------------------------------
-module('RPC').

-include("../../Foreign.hrl").

-export([ callTimeout/3
        , multiCallTimeout/3
        , receiveResponseTimeout/2
        , waitResponse/1
        , waitResponseTimeout/2
        ]).

-import('ToErl', [toErl/1]).

callTimeout(Node, Fun, Timeout) ->
  ?IO(erpc:call(Node, Fun, toErl(Timeout))).

multiCallTimeout(Nodes, Fun, Timeout) ->
  ?IO(erpc:multicall(Nodes, Fun, toErl(Timeout))).

receiveResponseTimeout(RequestId, Timeout) ->
  ?IO(erpc:receive_response(RequestId, toErl(Timeout))).

waitResponse(RequestId) ->
  ?IO(case erpc:wait_response(RequestId) of
        {response, Result} -> {'Just', Result};
        no_response -> {'Nothing'}
      end).

waitResponseTimeout(RequestId, Timeout) ->
  ?IO(case erpc:wait_response(RequestId, toErl(Timeout)) of
        {response, Result} -> {'Just', Result};
        no_response -> {'Nothing'}
      end).

