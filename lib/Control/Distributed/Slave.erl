%%---------------------------------------------------------------------------
%% |
%% Module      :  Slave
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Slave FFI module.
%%
%%---------------------------------------------------------------------------
-module('Slave').

-include("../../Foreign.hrl").

-export([ start/1
        , startWith/2
        , startWithArgs/3
        , startLink/1
        , startLinkWith/2
        , startLinkWithArgs/3
        ]).

start(Host) ->
  ?IO(return(slave:start(Host))).

startWith(Host, Name) ->
  ?IO(return(slave:start(Host, Name))).

startWithArgs(Host, Name, Args) ->
  ?IO(return(slave:start(Host, Name, Args))).

startLink(Host) ->
  ?IO(return(slave:start_link(Host))).

startLinkWith(Host, Name) ->
  ?IO(return(slave:start_link(Host, Name))).

startLinkWithArgs(Host, Name, Args) ->
  ?IO(return(slave:start_link(Host, Name, Args))).

return({ok, Node}) -> Node;
return({error, Reason}) -> error(Reason).
