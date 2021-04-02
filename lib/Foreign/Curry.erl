%%---------------------------------------------------------------------------
%% |
%% Module      :  Curry
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Curry module.
%%
%%---------------------------------------------------------------------------
-module('Curry').

-include("../Foreign.hrl").

-compile({no_auto_import, [apply/2]}).

-export([ curry/1
        , curryIO/1
        , apply/2
        , applyIO/2
        ]).

-include("./Curry.hrl").

applyIO(Fun, Args) ->
  apply(?RunIO(Fun), Args).

%% Apply a curried function
apply(Fun, [A|Args]) -> apply(Fun(A), Args);
apply(Ret, []) -> Ret.
