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

-export([ nodesOf/1
        , disconnect/1
        , getCookie/0
        , setCookie/2
        ]).

nodesOf(Type) -> erlang:nodes(toErl(Type)).

disconnect(Node) ->
  case erlang:disconnect_node(Node) of
    true    -> {'Just', true};
    false   -> {'Just', false};
    ignored -> {'Nothing'}
  end.

getCookie() -> atom_to_list(erlang:get_cookie()).

setCookie(Node, Cookie) ->
  erlang:set_cookie(Node, list_to_atom(Cookie)).

toErl({'VisibleNode'}) -> visible;
toErl({'HiddenNode'}) -> hidden;
toErl({'ConnectedNode'}) -> connected;
toErl({'ThisNode'}) -> this;
toErl({'KnownNode'}) -> known.
