%%---------------------------------------------------------------------------
%% |
%% Module      :  Port
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Port FFI module.
%%
%%---------------------------------------------------------------------------
-module('Port').

-include("../../Foreign.hrl").

-compile({no_auto_import, [link/1, unlink/1]}).

-export([ open/2
        , openDriver/2
        , openExe/2
        , openFd/3
        , command/2
        , connect/2
        , info/1
        , linkPort/1
        , unlinkPort/1
        ]).

open(Command, Settings) ->
  erlang:open_port({spawn, Command}, parseSettings(Settings)).

openDriver(Command, Settings) ->
  erlang:open_port({spawn_driver, Command}, parseSettings(Settings)).

openExe(Filename, Settings) ->
  erlang:open_port({spawn_executable, Filename}, parseSettings(Settings)).

openFd(In, Out, Settings) ->
  erlang:open_port({fd, In, Out}, parseSettings(Settings)).

parseSettings(Settings) ->
    parseSettings(maps:to_list(Settings), []).

parseSettings([{packet, 0}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{packet, I}|Settings], Result) when I == 1; I == 2; I == 4 ->
  parseSettings(Settings, [{packet, I}|Result]);
parseSettings([{packet, I}|_Settings], _Result) ->
  error({bad_packet_setting, I});
parseSettings([{stream, false}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{stream, true}|Settings], Result) ->
  parseSettings(Settings, [stream|Result]);
parseSettings([{line, 0}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{line, N}|Settings], Result) ->
  parseSettings(Settings, [{line, N}|Result]);
parseSettings([{cd, ""}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{cd, Cd}|Settings], Result) ->
  parseSettings(Settings, [{cd, Cd}|Result]);
parseSettings([{env, []}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{env, Env}|Settings], Result) ->
  parseSettings(Settings, [{env, Env}|Result]);
parseSettings([{args, []}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{args, Args}|Settings], Result) ->
  parseSettings(Settings, [{args, Args}|Result]);
parseSettings([{arg0, ""}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{arg0, Arg0}|Settings], Result) ->
  parseSettings(Settings, [{arg0, Arg0}|Result]);
parseSettings([{exitStatus, false}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{exitStatus, true}|Settings], Result) ->
  parseSettings(Settings, [exit_status|Result]);
parseSettings([{useStdio, true}|Settings], Result) ->
  parseSettings(Settings, [use_stdio|Result]);
parseSettings([{useStdio, false}|Settings], Result) ->
  parseSettings(Settings, [nouse_stdio|Result]);
parseSettings([{stderrToOut, false}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{stderrToOut, true}|Settings], Result) ->
  parseSettings(Settings, [stderr_to_stdout|Result]);
parseSettings([{overlappedIO, false}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{overlappedIO, true}|Settings], Result) ->
  parseSettings(Settings, [overlapped_io|Result]);
parseSettings([{direction, none}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{direction, in}|Settings], Result) ->
  parseSettings(Settings, [in|Result]);
parseSettings([{direction, out}|Settings], Result) ->
  parseSettings(Settings, [out|Result]);
parseSettings([{direction, both}|Settings], Result) ->
  parseSettings(Settings, [in, out|Result]);
parseSettings([{hide, true}|Settings], Result) ->
  parseSettings(Settings, [hide|Result]);
parseSettings([{hide, false}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{busyLimitsPort, {0, 0}}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{busyLimitsPort, {Low, High}}|Settings], Result) when Low < High ->
  parseSettings(Settings, [{busy_limits_port, {Low, High}}|Result]);
parseSettings([{busyLimitsMsgQ, {0, 0}}|Settings], Result) ->
  parseSettings(Settings, Result);
parseSettings([{busyLimitsMsgQ, {Low, High}}|Settings], Result) when Low < High ->
  parseSettings(Settings, [{busy_limits_msgq, {Low, High}}|Result]);
parseSettings([Bad|_], _Result) ->
  error({bad_settings, Bad});
parseSettings([], Result) -> [binary|Result].

command(Port, Data) ->
  ?IO(ok(erlang:port_command(Port, Data))).

connect(Port, Pid) ->
  ?IO(ok(erlang:port_connect(Port, Pid))).

info(Port) ->
  ?IO(case erlang:port_info(Port) of
        undefined -> ?Nothing;
        Info -> ?Just(infoRec(Info))
      end).

infoRec(Info) -> infoRec(Info, #{}).

infoRec([{id, Index}|Info], Rec) ->
  infoRec(Info, Rec#{id => Index});
infoRec([{name, Name}|Info], Rec) ->
  infoRec(Info, Rec#{name => Name});
infoRec([{connected, Pid}|Info], Rec) ->
  infoRec(Info, Rec#{connected => Pid});
infoRec([{links, Pids}|Info], Rec) ->
  infoRec(Info, Rec#{links => Pids});
infoRec([{input, Input}|Info], Rec) ->
  infoRec(Info, Rec#{input => Input});
infoRec([{output, Output}|Info], Rec) ->
  infoRec(Info, Rec#{output => Output});
infoRec([{os_pid, OsPid}|Info], Rec) ->
  infoRec(Info, Rec#{osPid => OsPid});
%%infoRec([{registered_name, Name}|Info], Rec) ->
%%  infoRec(Info, Rec#{registeredName => Name});
infoRec([_|Info], Rec) -> infoRec(Info, Rec);
infoRec([], Rec) -> Rec.

linkPort(Port) ->
  ?IO(ok(erlang:link(Port))).

unlinkPort(Port) ->
  ?IO(ok(erlang:unlink(Port))).

ok(true) -> ok.
