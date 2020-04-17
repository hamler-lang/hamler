%%---------------------------------------------------------------------------
%% |
%% Module      :  Monad
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Monad Module.
%%
%%---------------------------------------------------------------------------
-module('Monad').

-export([ pureImpl/1
        , bindImpl/2
        ]).

pureImpl(X) -> X.

bindImpl(X, F) -> F(X).
