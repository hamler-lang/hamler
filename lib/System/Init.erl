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

-export([ getArgument/1
        , getArguments/0
        , getStatus/0
        ]).

getArgument(Flag) ->
    case catch init:get_argument(list_to_existing_atom(Flag)) of
        {ok, Arg} -> {'Just', Arg};
        error -> {'Nothing'};
        {'EXIT', _} -> {'Nothing'}
    end.

getArguments() ->
    [{atom_to_list(Flag), Values} || {Flag, Values} <- init:get_arguments()].

getStatus() ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    {constr(InternalStatus), atom_to_list(ProvidedStatus)}.

constr(starting) -> {'Starting'};
constr(started)  -> {'Started'};
constr(stopping) -> {'Stopped'}.

