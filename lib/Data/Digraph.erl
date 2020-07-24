%%---------------------------------------------------------------------------
%% |
%% Module      :  Digraph
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
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

-include("Maybe.hrl").

%% FFI
-export([ new/1
        , addVertex/2
        , edge/2
        , vertex/2
        , getCycle/2
        , getPath/3
        , getShortCycle/2
        , getShortPath/3
        ]).

trans([]) -> [];
trans([x | xs]) -> [case x of
    { 'Cyclic' } -> digraph:cyclic();
    { 'Acyclic' } -> digraph:acyclic();
    { 'Protected' } -> digraph:protected();
    { 'Private' } -> digraph:private()
end | trans(xs) ].

new(type) -> digraph:new(trans(type)).

addVertex(g, labal) ->
    digraph:add_vertex(g, digraph:add_vertex(g), labal).

edge(g, e) -> case digraph:edge(g, e) of
    {e, v1, v2, labal} -> { 'Just', { v1, v2, labal } };
    false -> { 'Nothing' }
end.

vertex(g, v) -> case digraph:vertex(g, v) of
    {v, labal} -> { 'Just', labal };
    false -> { 'Nothing' }
end.

getCycle(g, v) -> case digraph:get_cycle(g, v) of
    false -> [];
    else -> else
end.

getPath(g, v1, v2) -> case digraph:get_path(g, v1, v2) of
    false -> [];
    else -> else
end.

getShortCycle(g, v) -> case digraph:get_short_cycle(g, v) of
    false -> [];
    else -> else
end.

getShortPath(g, v1, v2) -> case digraph:get_short_path(g, v1, v2) of
    false -> [];
    else -> else
end.
