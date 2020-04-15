%%---------------------------------------------------------------------------
%% |
%% Module      :  IO
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Erlang IO Module.
%%
%%---------------------------------------------------------------------------
-module('IO').

-export([print/1]).

print(S) -> io:format("~s", [S]), S.
