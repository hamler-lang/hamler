%%---------------------------------------------------------------------------
%% |
%% Module      :  Digraph
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Zhang Shiwei, zhangsw@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Digraph FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Digraph').

-include("../Foreign.hrl").

%% FFI
-export([ edgeOf/1
        , vertexOf/1
        , edgeID/1
        , vertexID/1
        , new/1
        , addEdge/4
        , modifyEdge/5
        , addVertex/2
        , edge/2
        , vertex/2
        , getCycle/2
        , getPath/3
        , getShortCycle/2
        , getShortPath/3
        , info/1
        , eqImpl/2
        ]).

-define(Left(V), {'Left', V}).
-define(Right(V), {'Right', V}).

edgeOf(ID) -> ['$e' | ID].
vertexOf(ID) -> ['$v' | ID].
edgeID(['$e' | ID]) -> ID.
vertexID(['$v' | ID]) -> ID.

eqImpl(X, Y) -> X =:= Y.

trans([]) -> [];
trans([X | Xs]) ->
  [case X of
     {'Cyclic'} -> digraph:cyclic();
     {'Acyclic'} -> digraph:acyclic();
     {'Protected'} -> digraph:protected();
     {'Private'} -> digraph:private()
   end | trans(Xs)].

new(Type) -> ?IO(digraph:new(trans(Type))).

addEdge(G, V1, V2, Labal) -> ?IO(
  case digraph:add_edge(G, V1, V2, Labal) of
    {error, {bad_edge, Path}} -> ?Left({'BadEdge', Path});
    {error, {bad_vertex, V}} -> ?Left({'BadVertex', V});
    V -> ?Right(V)
  end).

modifyEdge(G, E, V1, V2, Labal) -> ?IO(
  case digraph:add_edge(G, E, V1, V2, Labal) of
    {error, {bad_edge, Path}} -> ?Left({'BadEdge', Path});
    {error, {bad_vertex, V}} -> ?Left({'BadVertex', V});
    V -> ?Right(V)
  end).

addVertex(G, Labal) -> ?IO(
  digraph:add_vertex(G, digraph:add_vertex(G), Labal)).

edge(G, E) -> ?IO(
  case digraph:edge(G, E) of
    {E, V1, V2, Labal} -> ?Just({V1, V2, Labal});
    false -> ?Nothing
  end).

vertex(G, V) -> ?IO(
  case digraph:vertex(G, V) of
    {V, Labal} -> ?Just(Labal);
    false -> ?Nothing
  end).

getCycle(G, V) -> ?IO(
  case digraph:get_cycle(G, V) of
    false -> [];
    Else -> Else
  end).

getPath(G, V1, V2) -> ?IO(
  case digraph:get_path(G, V1, V2) of
    false -> [];
    Else -> Else
  end).

getShortCycle(G, V) -> ?IO(
  case digraph:get_short_cycle(G, V) of
    false -> [];
    Else -> Else
  end).

getShortPath(G, V1, V2) -> ?IO(
  case digraph:get_short_path(G, V1, V2) of
    false -> [];
    Else -> Else
  end).

info(G) -> ?IO(
  lists:map(fun({memory, M}) ->
                {'GraphMemoryInfo', M};
               ({cyclicity, Y}) ->
                {'GraphTypeInfo', {case Y of cyclic -> 'Cyclic'; acyclic -> 'Acyclic' end}};
               ({portection, Y}) ->
                {'GraphTypeInfo', {case Y of protected -> 'Protected'; private -> 'Private' end}}
            end, digraph:info(G))).
