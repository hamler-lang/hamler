%%---------------------------------------------------------------------------
%% |
%% Module      :  File
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
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

-export([ open/2
        , read/2
        , write/2
        , close/1
        ]).

open(Filename, Mode) ->
    return(file:open(Filename, modes(Mode))).

read(IoDevice, Lengh) ->
    case file:read(IoDevice, Lengh) of
        eof -> {'Nothing'};
        {ok, Data} -> {'Just', Data};
        {error, Reason} -> error(Reason)
    end.

write(IoDevice, Data) ->
    return(file:write(IoDevice, Data)).

close(IoDevice) ->
    return(file:close(IoDevice)).

modes({'ReadMode'}) -> [read];
modes({'WriteMode'}) -> [write];
modes({'AppendMode'}) -> [append];
modes({'ReadWriteMode'}) -> [read, write].

return(ok) -> ok;
return({ok, IoDevice}) -> IoDevice;
return({error, Reason}) -> error(Reason).

