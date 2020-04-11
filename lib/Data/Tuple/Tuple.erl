%%---------------------------------------------------------------------------
%% |
%% Module      :  Tuple
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Tuple FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Tuple').

-export([ elem/2
        , setElem/3
        , size/1
        ]).

-spec(elem(integer(), tuple()) -> any()).
elem(I, Tup) -> element(I, Tup).

-spec(setElem(integer(), any(), tuple()) -> tuple()).
setElem(I, El, Tup) -> setelement(I, El, Tup).

-spec(size(tuple()) -> integer()).
size(Tup) -> tuple_size(Tup).

