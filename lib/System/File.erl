%%---------------------------------------------------------------------------
%% |
%% Module      :  File
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The File FFI module.
%%
%%---------------------------------------------------------------------------
-module('File').

-include("../Foreign.hrl").

-export([ open/2
        , openRaw/2
        , read/2
        , readLine/1
        , seek/3
        , tell/1
        , write/2
        , close/1
        , sync/1
        ]).

open(File, Mode) ->
  ?IO(return(file:open(File, [binary|parseMode(Mode)]))).

openRaw(File, Mode) ->
  ?IO(return(file:open(File, [raw,binary|parseMode(Mode)]))).

read(IoDevice, Lengh) ->
  ?IO(return(file:read(IoDevice, Lengh))).

readLine(IoDevice) ->
  ?IO(return(file:read_line(IoDevice))).

seek(IoDevice, Mode, Offset) ->
  ?IO(return(file:position(IoDevice, location(Mode, Offset)))).

tell(IoDevice) ->
  ?IO(return(file:position(IoDevice, {cur, 0}))).

write(IoDevice, Data) ->
  ?IO(return(file:write(IoDevice, Data))).

close(IoDevice) ->
  ?IO(return(file:close(IoDevice))).

sync(IoDevice) ->
  ?IO(return(file:sync(IoDevice))).

-compile({inline, [parseMode/1]}).
parseMode({'ReadMode'}) -> [read];
parseMode({'WriteMode'}) -> [write];
parseMode({'AppendMode'}) -> [append];
parseMode({'ReadWriteMode'}) -> [read, write].

-compile({inline, [location/2]}).
location({'AbsoluteSeek'}, Offset) -> {bof, Offset};
location({'RelativeSeek'}, Offset) -> {cur, Offset};
location({'SeekFromEnd'}, Offset) -> {eof, Offset}.

-compile({inline, [return/1]}).
return(ok) -> ok;
return({ok, Result}) -> Result;
return(eof) -> error(eof);
return({error, Reason}) -> error(Reason).
