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

-export([ getPid/0
        , osName/0
        , osFamily/0
        , runCmd/1
        , setSignal/2
        , version/0
        ]).

getPid() ->
  ?IO(list_to_integer(os:getpid())).

osName() ->
  ?IO(atom_to_list(element(2, os:type()))).

osFamily() ->
  ?IO(atom_to_list(element(1, os:type()))).

setSignal(Signal, Option) ->
  ?IO(os:set_signal(destruct(Signal), destruct(Option))).

runCmd(Command) ->
  ?IO(os:cmd(Command)).

-spec(version() -> string()).
version() ->
  ?IO(case os:version() of
        {Major, Minor, Release} ->
          string:join([integer_to_list(I) || I <- [Major, Minor, Release]], ".");
        VersionString -> VersionString
      end).

destruct({'Sighup'})  -> sighup;
destruct({'Sigquit'}) -> sigquit;
destruct({'Sigabrt'}) -> sigabrt;
destruct({'Sigalrm'}) -> sigalrm;
destruct({'Sigterm'}) -> sigterm;
destruct({'Sigusr1'}) -> sigusr1;
destruct({'Sigusr2'}) -> sigusr2;
destruct({'Sigchld'}) -> sigchld;
destruct({'Sigstop'}) -> sigstop;
destruct({'Sigtstp'}) -> sigtstp;

destruct({'SigDefault'}) -> default;
destruct({'SigHandle'})  -> handle;
destruct({'SigIgnore'})  -> ignore.

