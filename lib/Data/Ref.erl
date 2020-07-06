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

-export([ makeRef/0
        , showRefImpl/1
        ]).

-spec(makeRef() -> reference()).
makeRef() -> erlang:make_ref().

-spec(showRefImpl(reference()) -> string()).
showRefImpl(Ref) -> ref_to_list(Ref).
