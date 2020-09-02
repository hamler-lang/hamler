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
%% The Enum FFI module.
%%
%%---------------------------------------------------------------------------
-module('Enum').

-export([ enumCharSucc/1
        , enumCharPred/1
        , enumCharRange/2
        , enumCharRangeStep/3
        , enumIntegerSucc/1
        , enumIntegerPred/1
        , enumIntegerRange/2
        , enumIntegerRangeStep/3
        , enumFloatSucc/1
        , enumFloatPred/1
        , enumFloatRange/2
        , enumFloatRangeStep/3
        ]).

enumCharSucc(X) -> X + 1.

enumCharPred(X) -> X - 1.

enumCharRange(Start, Stop) when Stop >= Start ->
  range(Start, Stop, 1);
enumCharRange(Start, Stop) when Stop < Start ->
  range(Start, Stop, -1).

enumCharRangeStep(Start, Next, Stop) ->
  range(Start, Stop, Next-Start).

enumIntegerSucc(X) -> X + 1.

enumIntegerPred(X) -> X - 1.

enumIntegerRange(Start, Stop) when Stop >= Start ->
  range(Start, Stop, 1);
enumIntegerRange(Start, Stop) when Stop < Start ->
  range(Start, Stop, -1).

enumIntegerRangeStep(Start, Next, Stop) ->
  range(Start, Stop, Next-Start).

enumFloatSucc(X) -> X + 1.

enumFloatPred(X) -> X - 1.

enumFloatRange(Start, Stop) when Stop >= Start ->
  floatRange(Start, Stop, 1);
enumFloatRange(Start, Stop) when Stop < Start ->
  floatRange(Start, Stop, -1).

enumFloatRangeStep(Start, Next, Stop) ->
  floatRange(Start, Stop, Next-Start).

floatRange(_, _, 0.0) -> error("bad argument");
floatRange(Start, Stop, Step) 
  when (Step > 0.0) and (Start > Stop) or (Step < 0.0) and (Start < Stop) -> [];
floatRange(Start, Stop, Step) -> 
  [Start | floatRange(Start + Step, Stop, Step)].

-compile({inline, [range/3]}).
range(Start, Stop, Step) ->
  lists:seq(Start, Stop, Step).
