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
%% The Init FFI module.
%%
%%---------------------------------------------------------------------------
-module('Init').

-export([ getArguments/0
        , getArgument/1
        , getStatus/0
        , restartWith/1
        ]).

getArguments() ->
  [{Flag, Values} || {Flag, Values} <- init:get_arguments()].

getArgument(Flag) ->
  case catch init:get_argument(list_to_existing_atom(Flag)) of
    {ok, Arg} -> {'Just', Arg};
    error -> {'Nothing'};
    {'EXIT', _} -> {'Nothing'}
  end.

getStatus() ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    {constr(InternalStatus), atom_to_list(ProvidedStatus)}.

%%---------------------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------------------

restartWith({'Embedded'}) ->
  init:restart([{mode, embedded}]);
restartWith({'Interactive'}) ->
  init:restart([{mode, interactive}]).

constr(starting) -> {'Starting'};
constr(started)  -> {'Started'};
constr(stopping) -> {'Stopped'}.

