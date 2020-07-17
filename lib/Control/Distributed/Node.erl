%%---------------------------------------------------------------------------
%% |
%% Module      :  Node
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Node FFI module.
%%
%%---------------------------------------------------------------------------
-module('Node').

-include("../../Foreign.hrl").

-export([ nodes/0
        , nodesOf/1
        , selfNode/0
        , nodeOfPid/1
        , nodeOfPort/1
        , nodeOfRef/1
        , disconnect/1
        , getCookie/0
        , setCookie/2
        , isAlive/0
        , monitorNode/2
        ]).

nodes() ->
  ?IO([name(N) || N <- erlang:nodes()]).

nodesOf(Type) ->
  ?IO([name(N) || N <- erlang:nodes(toErl(Type))]).

selfNode() ->
  ?IO(name(erlang:node())).

nodeOfPid(Pid) ->
  ?IO(name(erlang:node(Pid))).

nodeOfPort(Port) ->
  ?IO(name(erlang:node(Port))).

nodeOfRef(Ref) ->
  ?IO(name(erlang:node(Ref))).

disconnect(Node) ->
  ?IO(case erlang:disconnect_node(atom(Node)) of
        true    -> ?Just(true);
        false   -> ?Just(false);
        ignored -> ?Nothing
      end).

getCookie() ->
  ?IO(name(erlang:get_cookie())).

setCookie(Node, Cookie) ->
  ?IO(erlang:set_cookie(atom(Node), atom(Cookie))).

isAlive() -> ?IO(erlang:is_alive()).

monitorNode(Node, Flag) ->
  ?IO(erlang:monitor_node(atom(Node), Flag)).

atom(S) -> list_to_atom(S).
name(N) -> atom_to_list(N).

toErl({'VisibleNode'}) -> visible;
toErl({'HiddenNode'}) -> hidden;
toErl({'ConnectedNode'}) -> connected;
toErl({'ThisNode'}) -> this;
toErl({'KnownNode'}) -> known.
