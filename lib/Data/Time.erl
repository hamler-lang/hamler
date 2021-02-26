%%---------------------------------------------------------------------------
%% |
%% Module      :  Time
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Time FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Time').

-include("../Foreign.hrl").

-export([
    getCurrentTime/0, 
    getLocalTime/0,
    getIsoWeekNumber/0,
    getUniversalTime/0,
    rfc3339ToSeconds/1,
    rfc3339ToNanoseconds/1,
    rfc3339ToMilliseconds/1,
    rfc3339ToMicroseconds/1,
    systemTimeToLocalTime/2,
    systemTimeToUniversalTime/2,
    secondsToRfc3339/1,
    nanosecondsToRfc3339/1,
    millisecondsToRfc3339/1,
    microsecondsToRfc3339/1]).

-include("Time.hrl").

getCurrentTime() -> ?IO(erlang:timestamp()).
getLocalTime() -> ?IO(calendar:local_time()).
getIsoWeekNumber() -> ?IO(calendar:iso_week_number()).
getUniversalTime() -> ?IO(calendar:universal_time()).

rfc3339ToSeconds(S) -> 
    calendar:rfc3339_to_system_time(S, [{unit, second}]).
rfc3339ToNanoseconds(S) -> 
    calendar:rfc3339_to_system_time(S, [{unit, nanosecond}]).
rfc3339ToMilliseconds(S) -> 
    calendar:rfc3339_to_system_time(S, [{unit, millisecond}]).
rfc3339ToMicroseconds(S) -> 
    calendar:rfc3339_to_system_time(S, [{unit, microsecond}]).

systemTimeToLocalTime(S, U) ->
    calendar:system_time_to_local_time(S, time_unit(U)).

systemTimeToUniversalTime(S, U) ->
    calendar:system_time_to_universal_time(S, time_unit(U)).

secondsToRfc3339(S) ->
    calendar:system_time_to_rfc3339(S, [{unit, second}]).
nanosecondsToRfc3339(S) ->
    calendar:system_time_to_rfc3339(S, [{unit, nanosecond}]).
millisecondsToRfc3339(S) ->
    calendar:system_time_to_rfc3339(S, [{unit, millisecond}]).
microsecondsToRfc3339(S) ->
    calendar:system_time_to_rfc3339(S, [{unit, microsecond}]).
