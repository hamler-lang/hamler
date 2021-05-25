%%---------------------------------------------------------------------------
%% |
%% Module      :  Queue
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Queue FFI module.
%%
%%---------------------------------------------------------------------------
-module('Queue').

-include("../Foreign.hrl").

-export([ daeh/1
        , drop/1
        , out/1
        , outR/1
        , peek/1
        , peekR/1
        ]).

daeh(Q) ->
  try queue:daeh(Q) of
    E -> ?Just(E)
  catch error:empty ->
    ?Nothing
  end.

%% TODO: Fixme later.
drop(Q) -> queue:drop(Q).
out(Q) -> queue:out(Q).
outR(Q) -> queue:out_r(Q).
peek(Q) -> queue:peek(Q).
peekR(Q) -> queue:peek_r(Q).
