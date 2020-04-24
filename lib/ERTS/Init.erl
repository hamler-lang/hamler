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
%% The Init FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Init').

-export([ getStatus/0
        , getArguments/0
        , getArgument/1
        ]).

getStatus() ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    {constr(InternalStatus), atom_to_list(ProvidedStatus)}.

constr(starting) -> {'Starting'};
constr(started)  -> {'Started'};
constr(stopping) -> {'Stopped'}.

getArguments() ->
    [{atom_to_list(Flag), Values} || {Flag, Values} <- init:get_arguments()].

getArgument(Flag) ->
    case catch init:get_argument(list_to_existing_atom(Flag)) of
        {ok, Arg}   -> {'Just', Arg};
        error       -> {'Nothing'};
        {'EXIT', _} -> {'Nothing'}
    end.

