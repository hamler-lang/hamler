%%---------------------------------------------------------------------------
%% |
%% Module      :  Array
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Array FFI module.
%%
%%---------------------------------------------------------------------------
-module('QuickCheck').


-export([seed/1, mkRand/1, seed/3, split/1, next/1, uniform/1, randomRFloat/2,
         uniform_s/1, randomRInt/2, randomRChar/2, seed0/0]).

-define(PRIME1, 30269).
-define(PRIME2, 30307).
-define(PRIME3, 30323).

%%-----------------------------------------------------------------------
%% The type of the state

-type ran() :: {integer(), integer(), integer()}.

%%-----------------------------------------------------------------------

-spec seed0() -> ran().

seed0() ->
    {3172, 9814, 20125}.

%% seed()
%%  Seed random number generation with default values


%% seed({A1, A2, A3}) 
%%  Seed random number generation 

mkRand(V) ->
     seed(V).

seed(Int) when is_integer(Int) ->
    A1 = (Int bsr 16) band 16#fffffff,
    A2 = Int band 16#ffffff,
    A3 = (Int bsr 36) bor (A2 bsr 16),
    seed(A1, A2, A3);
seed({A1, A2, A3}) ->
    seed(A1, A2, A3).

%% seed(A1, A2, A3)
%%  Seed random number generation

seed(A1, A2, A3) ->
    ({(abs(A1) rem (?PRIME1-1)) + 1,   % Avoid seed numbers that are
	      (abs(A2) rem (?PRIME2-1)) + 1,   % even divisors of the
	      (abs(A3) rem (?PRIME3-1)) + 1}). % corresponding primes.


split({A1, A2, A3}) ->
    B1 = (A1*171) rem ?PRIME1,
    B2 = (A2*172) rem ?PRIME2,
    B3 = (A3*170) rem ?PRIME3,
    V1 = seed(B1*B2+B1+B2+1332292274972041455),
    V2 = seed(B2*B3+B2+B3+7304856964418773083),
    {V1, V2}.

next({A1, A2, A3}) ->
    B1 = (A1*171) rem ?PRIME1,
    B2 = (A2*172) rem ?PRIME2,
    B3 = (A3*170) rem ?PRIME3,
    {B1, B2, B3}.



%% uniform()
%%  Returns a random float between 0 and 1.


uniform({A1, A2, A3}) ->
    B1 = (A1*171) rem ?PRIME1,
    B2 = (A2*172) rem ?PRIME2,
    B3 = (A3*170) rem ?PRIME3,
    R = B1/?PRIME1 + B2/?PRIME2 + B3/?PRIME3,
    R - trunc(R).

%% uniform(N) -> I
%%  Given an integer N >= 1, uniform(N) returns a random integer
%%  between 1 and N.


randomRInt({A,B}, Seed) ->
    T = B-A+1,
    V = uniform(T, Seed),
    A+V-1.

randomRChar({A,B}, Seed) ->
    T = B-A+1,
    V = uniform(T, Seed),
    A+V-1.



randomRFloat({A,B}, Seed) ->
    T = B-A,
    V = uniform_s(Seed)*T+A,
    V.





uniform(N, Seed) when is_integer(N), N >= 1 ->
    trunc(uniform(Seed) * N) + 1.


%%% Functional versions

%% uniform_s(State) -> {F, NewState}
%%  Returns a random float between 0 and 1.

uniform_s({A1, A2, A3}) ->
    R = A1/?PRIME1 + A2/?PRIME2 + A3/?PRIME3,
    R - trunc(R).

%% uniform_s(N, State) -> {I, NewState}
%%  Given an integer N >= 1, uniform(N) returns a random integer
%%  between 1 and N.

%% uniform_s(N, State0) when is_integer(N), N >= 1 ->
%%     {F, State1} = uniform_s(State0),
%%     {trunc(F * N) + 1, State1}.





