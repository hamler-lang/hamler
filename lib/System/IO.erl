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
        ]).

-type(filepath() :: string()).

-spec(readFile(filepath()) -> ok).
readFile(FilePath) ->
  ?IO(return(file:read_file(FilePath))).

-spec(writeFile(filepath(), binary()) -> ok).
writeFile(FilePath, Data) ->
  ?IO(return(file:write_file(FilePath, Data, [write]))).

-spec(appendFile(filepath(), binary()) -> ok).
appendFile(FilePath, Data) ->
  ?IO(return(file:write_file(FilePath, Data, [append]))).

-compile({inline, [return/1]}).
return(ok) -> ok;
return({ok, Data}) -> Data;
return({error, Reason}) -> error(Reason).

