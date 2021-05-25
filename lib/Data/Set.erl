%%---------------------------------------------------------------------------
%% |
%% Module      :  Set
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Set FFI module.
%%
%%---------------------------------------------------------------------------
-module('Set').

-export([ singleton/1
        , fold/3
        ]).

-spec(singleton(term()) -> sets:set()).
singleton(A) -> sets:add_element(A, sets:new()).

fold(Fun, B0, Set) ->
    sets:fold(fun(A, B) -> (Fun(A))(B) end, B0, Set).