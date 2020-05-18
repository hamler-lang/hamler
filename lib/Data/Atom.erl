%%---------------------------------------------------------------------------
%% |
%% Module      :  Atom
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Atom FFI module.
%%
%%---------------------------------------------------------------------------
-module('Atom').

-export([ atom/1
        , toString/1
        , eqAtomImpl/2
        ]).

atom(S) -> list_to_atom(S).

toString(A) -> atom_to_list(A).

eqAtomImpl(A1, A2) -> A1 =:= A2.
