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
        ]).

trans([]) -> [];
trans([X | Xs]) -> [ case X of
    { 'Size', S } -> { size, S };
    { 'Fix', B } -> { fixed, B };
    { 'Default', V } -> { default, V }
end | trans(Xs) ].

new(Opt) -> array:new(trans(Opt)).
