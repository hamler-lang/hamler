%%---------------------------------------------------------------------------
%% |
%% Module      :  OS
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The OS FFI module.
%%
%%---------------------------------------------------------------------------
-module('OS').

-include("../Foreign.hrl").

-export([ findExe/1
        , findExeIn/2
        , getPid/0
        , osName/0
        , osFamily/0
        , runCmd/1
        , setSignal/2
        , systemTime/0
        , systemTimeIn/1
        , perfCounter/0
        , perfCounterIn/1
        , version/0
        ]).

findExe(Name) ->
  ?IO(case os:find_executable(Name) of
        false -> ?Nothing;
        Filename -> ?Just(Filename)
      end).

findExeIn(Name, Path) ->
  ?IO(case os:find_executable(Name, Path) of
        false -> ?Nothing;
        Filename -> ?Just(Filename)
      end).

getPid() ->
  ?IO(list_to_integer(os:getpid())).

osName() ->
  ?IO(atom_to_list(element(2, os:type()))).

osFamily() ->
  ?IO(atom_to_list(element(1, os:type()))).

runCmd(Command) -> ?IO(os:cmd(Command)).

setSignal(Signal, Option) ->
  ?IO(os:set_signal(unwrap(Signal), unwrap(Option))).

systemTime() ->
  ?IO(os:system_time()).

systemTimeIn(Unit) ->
  ?IO(os:system_time(unwrap(Unit))).

perfCounter() ->
  ?IO(os:perf_counter()).

perfCounterIn(Unit) ->
  ?IO(os:perf_counter(unwrap(Unit))).

-spec(version() -> string()).
version() ->
  ?IO(case os:version() of
        {Major, Minor, Release} ->
          string:join([integer_to_list(I) || I <- [Major, Minor, Release]], ".");
        VersionString -> VersionString
      end).

unwrap({'Second'}) -> second;
unwrap({'Millisecond'}) -> millisecond;
unwrap({'Microsecond'}) -> microsecond;
unwrap({'Nanosecond'}) -> nanosecond;
unwrap({'Native'}) -> native;
unwrap({'PerfCounter'}) -> perf_counter;

unwrap({'Sighup'})  -> sighup;
unwrap({'Sigquit'}) -> sigquit;
unwrap({'Sigabrt'}) -> sigabrt;
unwrap({'Sigalrm'}) -> sigalrm;
unwrap({'Sigterm'}) -> sigterm;
unwrap({'Sigusr1'}) -> sigusr1;
unwrap({'Sigusr2'}) -> sigusr2;
unwrap({'Sigchld'}) -> sigchld;
unwrap({'Sigstop'}) -> sigstop;
unwrap({'Sigtstp'}) -> sigtstp;
unwrap({'SigDefault'}) -> default;
unwrap({'SigHandle'})  -> handle;
unwrap({'SigIgnore'})  -> ignore.
