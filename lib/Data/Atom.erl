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
        , existingAtom/1
        , toString/1
        , eqAtomImpl/2
        , cmpAtomImpl/3
        ]).

-spec(atom(string()) -> atom()).
atom(S) -> list_to_atom(S).

-spec(existingAtom(string()) -> atom()).
existingAtom(S) -> list_to_existing_atom(S).

-spec(toString(atom()) -> string()).
toString(A) -> atom_to_list(A).

