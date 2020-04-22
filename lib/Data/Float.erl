%%---------------------------------------------------------------------------
%% |
%% Module      :  Float
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Float FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Float').

-export([ abs/1
        , acos/1
        , acosh/1
        , asin/1
        , asinh/1
        , atan/1
        , atan2/2
        , atanh/1
        , ceil/1
        , cos/1
        , cosh/1
        , erf/1
        , erfc/1
        , exp/1
        , floor/1
        , fmod/2
        , log/1
        , log2/1
        , log10/1
        , pi/0
        , pow/2
        , sin/1
        , sinh/1
        , sqrt/1
        , tan/1
        , tanh/1
        ]).

-spec(abs(float()) -> float()).
abs(X) -> erlang:abs(X).

-spec(acos(float()) -> float()).
acos(X) -> math:acos(X).

-spec(acosh(float()) -> float()).
acosh(X) -> math:acosh(X).

-spec(asin(float()) -> float()).
asin(X) -> math:asin(X).

-spec(asinh(float()) -> float()).
asinh(X) -> math:asinh(X).

-spec(atan(float()) -> float()).
atan(X) -> math:atan(X).

-spec(atan2(float(), float()) -> float()).
atan2(Y, X) -> math:atan2(Y, X).

-spec(atanh(float()) -> float()).
atanh(X) -> math:atanh(X).

-spec(ceil(float()) -> float()).
ceil(X) -> math:ceil(X).

-spec(cos(float()) -> float()).
cos(X) -> math:cos(X).

-spec(cosh(float()) -> float()).
cosh(X) -> math:cosh(X).

-spec(erf(float()) -> float()).
erf(X) -> math:erf(X).

-spec(erfc(float()) -> float()).
erfc(X) -> math:erfc(X).

-spec(exp(float()) -> float()).
exp(X) -> math:exp(X).

-spec(floor(float()) -> float()).
floor(X) -> math:floor(X).

-spec(fmod(float(), float()) -> float()).
fmod(X, Y) -> math:fmod(X, Y).

-spec(log(float()) -> float()).
log(X) -> math:log(X).

-spec(log2(float()) -> float()).
log2(X) -> math:log2(X).

-spec(log10(float()) -> float()).
log10(X) -> math:log10(X).

-spec(pow(float(), float()) -> float()).
pow(X, Y) -> math:pow(X, Y).

-spec(sin(float()) -> float()).
sin(X) -> math:sin(X).

-spec(sinh(float()) -> float()).
sinh(X) -> math:sinh(X).

-spec(sqrt(float()) -> float()).
sqrt(X) -> math:sqrt(X).

-spec(tan(float()) -> float()).
tan(X) -> math:tan(X).

-spec(tanh(float()) -> float()).
tanh(X) -> math:tanh(X).

-spec(pi() -> float()).
pi() -> 3.1415926535897932.

