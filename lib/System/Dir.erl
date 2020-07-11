%%---------------------------------------------------------------------------
%% |
%% Module      :  Dir
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Directory FFI module.
%%
%%---------------------------------------------------------------------------
-module('Dir').

-include("../Foreign.hrl").

-export([mkdir/1]).

%% mkdir :: FilePath -> IO ()
mkdir(FilePath) ->
  ?IO(case file:make_dir(FilePath) of
        ok -> ok;
        {error, Reason} -> error(Reason)
      end).
