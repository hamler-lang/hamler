%%---------------------------------------------------------------------------
%% |
%% Module      :  Error
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Error FFI module.
%%
%%---------------------------------------------------------------------------
-module('Error').

-export([showErrorImpl/1]).

-export([ throwException/1
        , catchException/2
        ]).

showErrorImpl(Error) ->
    lists:flatten(io_lib:format("~p", [Error])).

throwException(Ex) -> throw(Ex).

catchException(_X, _Y) -> ok.
