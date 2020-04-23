%%---------------------------------------------------------------------------
%% |
%% Module      :  Mnesia
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Mnesia Database FFI.
%%
%%---------------------------------------------------------------------------
-module('Mnesia').

-export([createTable/2]).

createTable(Name, TabDef) ->
    mnesia:create_table(list_to_atom(Name), parseTabDef(TabDef)).

parseTabDef(#{}) -> [].

