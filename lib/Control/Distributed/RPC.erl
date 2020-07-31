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

-export([ waitResponse/1
        , waitResponseTimeout/2
        ]).

waitResponse(RequestId) ->
  ?IO(case erpc:wait_response(RequestId) of
        {response, Result} -> ?Just(Result);
        no_response -> ?Nothing
      end).

waitResponseTimeout(RequestId, Timeout) ->
  ?IO(case erpc:wait_response(RequestId, Timeout) of
        {response, Result} -> ?Just(Result);
        no_response -> ?Nothing
      end).
