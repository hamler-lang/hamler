%%---------------------------------------------------------------------------
%% |
%% Module      :  Port
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Port FFI module.
%%
%%---------------------------------------------------------------------------
-module('Port').

-include("../../Foreign.hrl").

-export([ linkPort/1
        , unlinkPort/1
        ]).

linkPort(Port) ->
  ?IO(ok(erlang:link(Port))).

unlinkPort(Port) ->
  ?IO(ok(erlang:unlink(Port))).

ok(true) -> ok.
