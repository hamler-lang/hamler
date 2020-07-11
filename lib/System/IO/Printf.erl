%%---------------------------------------------------------------------------
%% |
%% Module      :  Printf
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Printf FFI module.
%%
%%---------------------------------------------------------------------------
-module('Printf').

-export([ print/1
        , println/1
        ]).

-spec(print(string()) -> ok).
print(S) -> fun() -> io:format(S) end.

-spec(println(string()) -> ok).
println(S) -> fun() -> io:format(S ++ "~n") end.

