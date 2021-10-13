%%---------------------------------------------------------------------------
%% |
%% Module      :  TCP
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%%                firest, shuai.wen@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The TCP FFI module.
%%
%%---------------------------------------------------------------------------
-module('Coerce').

-export([unsafeCoerce/1]).

unsafeCoerce(X) ->
    X.
