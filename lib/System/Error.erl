%%---------------------------------------------------------------------------
%% |
%% Module      :  ErrorE
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
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
        , finally/2
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

bracket(X, Y, Z) ->
  X2 = X(),
  try
    Y(X2)
  catch
        throw:A -> throw(A);
        error:B -> error(B);
        exit:C  -> exit(C)
  after
    Z(X2)
  end.

finally(X, Y) ->
  try X() of
    Z -> ?IO(Z)
  after Y()
  end.
