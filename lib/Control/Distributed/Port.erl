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
        , link/1
        , unlink/1
        ]).

open(Command, Settings) ->
  erlang:open_port({spawn, Command}, parse_settings(Settings)).

openDriver(Command, Settings) ->
  erlang:open_port({spawn_driver, Command}, parse_settings(Settings)).

openExe(Filename, Settings) ->
  erlang:open_port({spawn_executable, Filename}, parse_settings(Settings)).

openFd(In, Out, Settings) ->
  erlang:open_port({fd, In, Out}, parse_settings(Settings)).

parse_settings(Settings) ->
    parse_settings(maps:to_list(Settings), []).

parse_settings([{packet, 0}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{packet, I}|Settings], Result) when I == 1; I == 2; I == 4 ->
  parse_settings(Settings, [{packet, I}|Result]);
parse_settings([{packet, I}|_Settings], _Result) ->
  error({bad_packet_setting, I});
parse_settings([{stream, false}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{stream, true}|Settings], Result) ->
  parse_settings(Settings, [stream|Result]);
parse_settings([{line, 0}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{line, N}|Settings], Result) ->
  parse_settings(Settings, [{line, N}|Result]);
parse_settings([{cd, ""}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{cd, Cd}|Settings], Result) ->
  parse_settings(Settings, [{cd, Cd}|Result]);
parse_settings([{env, []}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{env, Env}|Settings], Result) ->
  parse_settings(Settings, [{env, Env}|Result]);
parse_settings([{args, []}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{args, Args}|Settings], Result) ->
  parse_settings(Settings, [{args, Args}|Result]);
parse_settings([{arg0, ""}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{arg0, Arg0}|Settings], Result) ->
  parse_settings(Settings, [{arg0, Arg0}|Result]);
parse_settings([{exitStatus, false}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{exitStatus, true}|Settings], Result) ->
  parse_settings(Settings, [exit_status|Result]);
parse_settings([{useStdio, true}|Settings], Result) ->
  parse_settings(Settings, [use_stdio|Result]);
parse_settings([{useStdio, false}|Settings], Result) ->
  parse_settings(Settings, [nouse_stdio|Result]);
parse_settings([{stderrToOut, false}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{stderrToOut, true}|Settings], Result) ->
  parse_settings(Settings, [stderr_to_stdout|Result]);
parse_settings([{overlappedIO, false}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{overlappedIO, true}|Settings], Result) ->
  parse_settings(Settings, [overlapped_io|Result]);
parse_settings([{direction, none}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{direction, in}|Settings], Result) ->
  parse_settings(Settings, [in|Result]);
parse_settings([{direction, out}|Settings], Result) ->
  parse_settings(Settings, [out|Result]);
parse_settings([{direction, both}|Settings], Result) ->
  parse_settings(Settings, [in, out|Result]);
parse_settings([{hide, true}|Settings], Result) ->
  parse_settings(Settings, [hide|Result]);
parse_settings([{hide, false}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{busyLimitsPort, {0, 0}}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{busyLimitsPort, {Low, High}}|Settings], Result) when Low < High ->
  parse_settings(Settings, [{busy_limits_port, {Low, High}}|Result]);
parse_settings([{busyLimitsMsgQ, {0, 0}}|Settings], Result) ->
  parse_settings(Settings, Result);
parse_settings([{busyLimitsMsgQ, {Low, High}}|Settings], Result) when Low < High ->
  parse_settings(Settings, [{busy_limits_msgq, {Low, High}}|Result]);
parse_settings([Bad|_], _Result) ->
  error({bad_settings, Bad});
parse_settings([], Result) -> [binary|Result].

command(Port, Data) ->
  ?IO(ok(erlang:port_command(Port, Data))).

connect(Port, Pid) ->
  ?IO(ok(erlang:port_connect(Port, Pid))).

info(Port) ->
  case erlang:port_info(Port) of
    undefined -> ?Nothing;
    Info -> ?Just(infoRec(Info))
  end.

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

link(Port) ->
  ?IO(ok(erlang:link(Port))).

unlink(Port) ->
  ?IO(ok(erlang:unlink(Port))).

ok(true) -> ok.
