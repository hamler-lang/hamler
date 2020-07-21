%%---------------------------------------------------------------------------
%% |
%% Module      :  IO
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The IO FFI module.
%%
%%---------------------------------------------------------------------------
-module('IO').

-include("../Foreign.hrl").

-export([ readFile/1
        , writeFile/2
        , appendFile/2
        , withFile/3
        , consultFile/1
        ]).

-import('System.File', [open/2]).

readFile(FilePath) ->
  ?IO(return(file:read_file(FilePath))).

writeFile(FilePath, Data) ->
  ?IO(return(file:write_file(FilePath, Data, [write]))).

appendFile(FilePath, Data) ->
  ?IO(return(file:write_file(FilePath, Data, [append]))).

withFile(FilePath, Mode, Fun) ->
  ?IO(case ?RunIO(open(FilePath, Mode)) of
        {ok, IoDevice} ->
          try Fun(IoDevice) after file:close(IoDevice) end;
        {error, Reason} -> error(Reason)
      end).

consultFile(FilePath) ->
  ?IO(return(file:consult(FilePath))).

-compile({inline, [return/1]}).
return(ok) -> ok;
return({ok, Data}) -> Data;
return({error, Reason}) -> error(Reason).
