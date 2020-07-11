%%---------------------------------------------------------------------------
%% |
%% Module      :  Ref
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Ref FFI module.
%%
%%---------------------------------------------------------------------------
-module('Ref').

-include("../Foreign.hrl").

-export([ makeRef/0
        , eqRefImpl/2
        , cmpRefImpl/3
        , showRefImpl/1
        ]).

makeRef() -> ?IO(erlang:make_ref()).

-spec(eqRefImpl(reference(), reference()) -> boolean()).
eqRefImpl(Ref1, Ref2) -> Ref1 == Ref2.

cmpRefImpl(LT, EQ, GT) ->
  fun(Ref1) ->
      fun(Ref2) ->
          if Ref1 > Ref2 -> GT;
             Ref1 < Ref2 -> LT;
             true -> EQ
          end
      end
  end.

-spec(showRefImpl(reference()) -> string()).
showRefImpl(Ref) -> ref_to_list(Ref).
