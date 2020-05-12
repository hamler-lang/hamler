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

enumCharRange(Start, End) when End >= Start ->
    range(Start, End, 1);
enumCharRange(Start, End) when End < Start ->
    range(Start, End, -1).

enumCharRangeStep(Start, End, Step) ->
    range(Start, End, Step).

enumIntegerRange(Start, End) when End >= Start ->
    range(Start, End, 1);
enumIntegerRange(Start, End) when End < Start ->
    range(Start, End, -1).

enumIntegerRangeStep(Start, End, Step) ->
    range(Start, End, Step).

-compile({inline, [range/3]}).
range(Start, End, Step) -> lists:seq(Start, End, Step).

