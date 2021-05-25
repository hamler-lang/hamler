%%---------------------------------------------------------------------------
%% |
%% Module      :  Node
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
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

-export([ nodesOf/1
        , disconnect/1
        , getCookie/0
        , setCookie/2
        , isAlive/0
        , monitorNode/2
        ]).

nodesOf(Type) ->
  ?IO(erlang:nodes(atom(Type))).

disconnect(Node) ->
  ?IO(case erlang:disconnect_node(Node) of
        true    -> ?Just(true);
        false   -> ?Just(false);
        ignored -> ?Nothing
      end).

getCookie() ->
  ?IO(atom_to_list(erlang:get_cookie())).

setCookie(Node, Cookie) ->
  ?IO(erlang:set_cookie(Node, atom(Cookie))).

isAlive() -> ?IO(erlang:is_alive()).

monitorNode(Node, Flag) ->
  ?IO(erlang:monitor_node(Node, Flag)).

atom({'Visible'}) -> visible;
atom({'Hidden'}) -> hidden;
atom({'Connected'}) -> connected;
atom({'Known'}) -> known;
atom(S) -> list_to_atom(S).
