%%---------------------------------------------------------------------------
%% |
%% Module      :  Init
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The System Init FFI module.
%%
%%---------------------------------------------------------------------------
-module('Init').

-include("../Foreign.hrl").

-export([ getArguments/0
        , getArgument/1
        , getStatus/0
        , restartWith/1
        ]).

getArguments() ->
  ?IO([{Flag, Values} || {Flag, Values} <- init:get_arguments()]).

getArgument(Flag) ->
  ?IO(case catch init:get_argument(list_to_existing_atom(Flag)) of
        {ok, Arg} -> {'Just', Arg};
        error -> {'Nothing'};
        {'EXIT', _} -> {'Nothing'}
      end).

getStatus() -> ?IO(init:get_status()).

%%---------------------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------------------

restartWith({'Embedded'}) ->
  init:restart([{mode, embedded}]);
restartWith({'Interactive'}) ->
  init:restart([{mode, interactive}]).
