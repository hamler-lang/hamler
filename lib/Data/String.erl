%%---------------------------------------------------------------------------
%% |
%% Module      :  String
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The String Module.
%%
%%---------------------------------------------------------------------------
-module('String').

-export([ connect/2
        , reverse/1
        , append/2
        , strlen/1
        ]).

%% connect :: String -> String -> String
-spec(connect(string(), string()) -> string()).
connect(S1, S2) -> S1 ++ S2.

%% reverse :: String -> String
-spec(reverse(string()) -> string()).
reverse(S) -> lists:reverse(S).

-spec(append(string(), string()) -> string()).
append(S1, S2) -> lists:append(S1, S2).

-spec(strlen(string()) -> integer()).
strlen(S) -> string:len(S).
