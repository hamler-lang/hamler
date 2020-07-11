%%---------------------------------------------------------------------------
%% |
%% Module      :  Enum
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Enum FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Enum').

-export([ enumCharRange/2
        , enumCharRangeStep/3
        , enumIntegerRange/2
        , enumIntegerRangeStep/3
        ]).

enumCharRange(Start, Stop) when Stop >= Start ->
    range(Start, Stop, 1);
enumCharRange(Start, Stop) when Stop < Start ->
    range(Start, Stop, -1).

enumCharRangeStep(Start, Next, Stop) ->
    range(Start, Stop, Next-Start).

enumIntegerRange(Start, Stop) when Stop >= Start ->
    range(Start, Stop, 1);
enumIntegerRange(Start, Stop) when Stop < Start ->
    range(Start, Stop, -1).

enumIntegerRangeStep(Start, Next, Stop) ->
    range(Start, Stop, Next-Start).

-compile({inline, [range/3]}).
range(Start, Stop, Step) ->
    lists:seq(Start, Stop, Step).
