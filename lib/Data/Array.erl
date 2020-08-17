%%---------------------------------------------------------------------------
%% |
%% Module      :  Array
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Array FFI module.
%%
%%---------------------------------------------------------------------------
-module('Array').

-export([ new/1
        , arrayFoldl/3
        , arrayFoldr/3
        , sparseFoldl/3
        , sparseFoldr/3
        , sparseMap/2
        ]).

new(Opt) -> array:new(trans(Opt)).

trans([]) -> [];
trans([X | Xs]) -> [ case X of
    { 'Size', S } -> { size, S };
    { 'Fix', B } -> { fixed, B };
    { 'Default', V } -> { default, V }
end | trans(Xs) ].

arrayFoldl(Fun, B0, Arr) ->
    array:foldl(fun(I, A, B) -> ((Fun(I))(B))(A) end, B0, Arr ).

arrayFoldr(Fun, B0, Arr) ->
    array:foldr(fun(I, A, B) -> ((Fun(I))(A))(B) end, B0, Arr ).

sparseFoldl(Fun, B0, Arr) ->
    array:sparse_foldl(fun(I, A, B) -> ((Fun(I))(B))(A) end, B0, Arr ).

sparseFoldr(Fun, B0, Arr) ->
    array:sparse_foldr(fun(I, A, B) -> ((Fun(I))(A))(B) end, B0, Arr ).

sparseMap(Fun, Arr) ->
    array:sparse_map(fun(I, A) -> (Fun(I))(A) end, Arr ).