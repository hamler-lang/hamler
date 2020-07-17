%%---------------------------------------------------------------------------
%% |
%% Module      :  NetKernel
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The NetKernel FFI module.
%%
%%---------------------------------------------------------------------------
-module('NetKernel').

-include("../../Foreign.hrl").

-export([ allow/1
        , connectNode/1
        , getTicktime/0
        , monitorNodes/1
        , monitorNodesOf/2
        , setTicktime/1
        , start/1
        , startWithType/2
        , startWithTicktime/3
        ]).

allow(Nodes) ->
  ?IO(net_kernel:allow([atom(N) || N <- Nodes])).

connectNode(Node) ->
  ?IO(case net_kernel:connect_node(atom(Node)) of
        ignored -> error(ignored);
        Bool -> Bool
      end).

getTicktime() ->
  ?IO(case net_kernel:get_net_ticktime() of
        Ticktime when is_integer(Ticktime) -> Ticktime;
        {ongoing_change_to, Ticktime} -> Ticktime;
        ignored -> error(ignored)
      end).

monitorNodes(Flag) ->
  ?IO(case net_kernel:monitor_nodes(Flag) of
        ok -> ok;
        error -> error(ignored);
        {error, Reason} -> error(Reason)
      end).

monitorNodesOf(Flag, NodeType) ->
  Options = [{node_type, toErl(NodeType)}],
  ?IO(case net_kernel:monitor_nodes(Flag, Options) of
        ok -> ok;
        error -> error(ignored);
        {error, Reason} -> error(Reason)
      end).

%% TODO:...
setTicktime(Ticktime) ->
  ?IO(case net_kernel:set_net_ticktime(Ticktime) of _Any -> ok end).

start(Name) ->
  ?IO(case net_kernel:start([atom(Name)]) of
        {ok, Pid} -> Pid;
        {error, Reason} -> error(Reason)
      end).

startWithType(Name, NameType) ->
  ?IO(case net_kernel:start([atom(Name), toErl(NameType)]) of
        {ok, Pid} -> Pid;
        {error, Reason} -> error(Reason)
      end).

startWithTicktime(Name, NameType, Ticktime) ->
  ?IO(case net_kernel:start([atom(Name), toErl(NameType), Ticktime]) of
        {ok, Pid} -> Pid;
        {error, Reason} -> error(Reason)
      end).

atom(Name) -> list_to_atom(Name).

toErl({'AllNode'}) -> all;
toErl({'HiddenNode'}) -> hidden;
toErl({'VisibleNode'}) -> visible;
toErl({'ShortName'}) -> shortnames;
toErl({'LongName'}) -> longnames.
