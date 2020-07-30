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
        ]).

showErrorImpl(Error) ->
  lists:flatten(io_lib:format("~p", [Error])).

throwException(Ex) -> ?IO(throw(Ex)).

catchException(X, Y) -> try X() of
                          Z -> ?IO(Z)
                        catch
                          throw:Throw -> Y(Throw);
                          error:Error -> Y(Error);
                          exit:Exit   -> Y(Exit)
                        end.

bracket(X, Y, Z) ->
  try X() of
    Result  ->
      ?IO(Y(Result))
  catch
        throw:A -> throw(A);
        error:B -> error(B);
        exit:C  -> exit(C);
  after
    ?IO((Z(Result))())
  end.
