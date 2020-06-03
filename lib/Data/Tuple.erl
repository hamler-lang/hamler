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
%% The Tuple FFI module.
%%
%%---------------------------------------------------------------------------
-module('Tuple').

%% Deprecated later.

-export([ elem/2
        , setElem/3
        , size/1
        ]).

-spec(elem(integer(), tuple()) -> any()).
elem(I, T) -> element(I, T).

-spec(setElem(integer(), any(), tuple()) -> tuple()).
setElem(I, E, T) -> setelement(I, T, E).

-spec(size(tuple()) -> integer()).
size(T) -> tuple_size(T).
