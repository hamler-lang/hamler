%%---------------------------------------------------------------------------
%% |
%% Module      :  Error
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Error FFI module.
%%
%%---------------------------------------------------------------------------
-module('Error').

-include("../Foreign.hrl").

-export([showErrorImpl/1]).

-export([ throwException/1
        , catchException/2
        , bracket/3
        , bracketOnError/3
        , finally/2
        , onException/2
        ]).

showErrorImpl(Error) ->
  lists:flatten(io_lib:format("~p", [Error])).

throwException(Ex) -> ?IO(throw(Ex)).

catchException(X, Y) -> try X() of
                          _Z -> ?IO(_Z)
                        catch
                          throw:_Throw -> Y(_Throw);
                          error:_Error -> Y(_Error);
                          exit:_Exit   -> Y(_Exit)
                        end.

bracket(Before, After, Thing) ->
    Resource = ?RunIO(Before),
    try
        ?EvalIO(Thing(Resource))
    catch _:_ ->
            ?EvalIO(After(Resource))
    after
        ?EvalIO(After(Resource))
    end.

bracketOnError(Before, After, Thing) ->
    Resource = ?RunIO(Before),
    try
        ?EvalIO(Thing(Resource))
    catch _:_ ->
            ?EvalIO(After(Resource))
    end.

finally(First, Second) ->
    try
        ?EvalIO(First)
    catch _:_ ->
            ?EvalIO(Second)
    after
        ?EvalIO(Second)
    end.

onException(First, Second) ->
    try
        ?EvalIO(First)
    catch _:_ ->
            ?EvalIO(Second)
    end.
