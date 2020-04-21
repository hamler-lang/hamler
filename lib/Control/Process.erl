%%---------------------------------------------------------------------------
%% |
%% Module      :  Process
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Process Module.
%%
%%---------------------------------------------------------------------------
-module('Process').

-export([ send/2
        , 'receive'/0
        , 'link'/1
        , 'monitor'/1
        ]).

-spec(send(pid(), term()) -> term()).
send(Pid, Msg) -> erlang:send(Pid, Msg).

-spec('receive'() -> term()).
'receive'() -> receive X -> X end.

-spec('link'(pid()) -> boolean()).
'link'(Pid) -> erlang:link(Pid).

-spec('monitor'(pid()) -> reference()).
'monitor'(Pid) -> erlang:monitor(process, Pid).
