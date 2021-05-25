%%---------------------------------------------------------------------------
%% |
%% Module      :  NetKernel
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
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

-export([ connectNode/1
        , getTicktime/0
        , monitorNodes/1
        , monitorNodesOf/2
        , setTicktime/1
        , start/1
        , startWithType/2
        , startWithTicktime/3
        ]).

connectNode(Node) ->
  ?IO(case net_kernel:connect_node(Node) of
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
  Options = [{node_type, atom(NodeType)}],
  ?IO(case net_kernel:monitor_nodes(Flag, Options) of
        ok -> ok;
        error -> error(ignored);
        {error, Reason} -> error(Reason)
      end).

%% TODO:...
setTicktime(Ticktime) ->
  ?IO(case net_kernel:set_net_ticktime(Ticktime) of _Any -> ok end).

start(Name) ->
  ?IO(case net_kernel:start([Name]) of
        {ok, Pid} -> Pid;
        {error, Reason} -> error(Reason)
      end).

startWithType(Name, NameType) ->
  ?IO(case net_kernel:start([Name, atom(NameType)]) of
        {ok, Pid} -> Pid;
        {error, Reason} -> error(Reason)
      end).

startWithTicktime(Name, NameType, Ticktime) ->
  ?IO(case net_kernel:start([Name, atom(NameType), Ticktime]) of
        {ok, Pid} -> Pid;
        {error, Reason} -> error(Reason)
      end).

atom({'Hidden'}) -> hidden;
atom({'Visible'}) -> visible;
atom({'ShortName'}) -> shortnames;
atom({'LongName'}) -> longnames.
