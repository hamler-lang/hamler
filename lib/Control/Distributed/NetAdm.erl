%%---------------------------------------------------------------------------
%% |
%% Module      :  NetAdm
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The NetAdm FFI module.
%%
%%---------------------------------------------------------------------------
-module('NetAdm').

-include("../../Foreign.hrl").

-export([ localhost/0
        , names/0
        , namesAt/1
        , ping/1
        ]).

localhost() -> ?IO(net_adm:localhost()).

names() ->
  ?IO(case net_adm:names() of
        {ok, List} -> List;
        {error, Reason} -> error(Reason)
      end).

namesAt(Host) ->
  ?IO(case net_adm:names(Host) of
        {ok, List} -> List;
        {error, Reason} -> error(Reason)
      end).

ping(Node) ->
  ?IO(case net_adm:ping(Node) of
        pong -> true;
        pang -> false
      end).
