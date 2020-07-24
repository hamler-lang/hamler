%%---------------------------------------------------------------------------
%% |
%% Module      :  Inet
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Inet FFI module.
%%
%%---------------------------------------------------------------------------
-module('Inet').

-include("../Foreign.hrl").

-export([ hostname/0
        , getStat/1
        , getStatWith/2
        , peername/1
        , sockname/1
        ]).

hostname() ->
  ?IO(return(inet:gethostname())).

getStat(Socket) ->
  ?IO(transStat(return(inet:getstat(Socket)))).

getStatWith(Socket, Options) ->
  ?IO(transStat(return(inet:getstat(Socket, Options)))).

peername(Socket) ->
  ?IO(transAddr(return(inet:peername(Socket)))).

sockname(Socket) ->
  ?IO(transAddr(return(inet:sockname(Socket)))).

transStat(Stats) when is_list(Stats) ->
  [{transStat(K), V} || {K, V} <- Stats];
transStat(recv_cnt)  -> {'RecvCnt'};
transStat(recv_max)  -> {'RecvMax'};
transStat(recv_avg)  -> {'RecvAvg'};
transStat(recv_oct)  -> {'RecvOct'};
transStat(recv_dvi)  -> {'RecvDvi'};
transStat(send_cnt)  -> {'SendCnt'};
transStat(send_max)  -> {'SendMax'};
transStat(send_avg)  -> {'SendAvg'};
transStat(send_oct)  -> {'SendOct'};
transStat(send_pend) -> {'SendPend'}.

transAddr({Addr, PortNumber}) ->
  {transAddr(Addr), PortNumber};
transAddr(Addr) when size(Addr) == 4 ->
  {'Ip4Address', Addr};
transAddr(Addr) when size(Addr) == 8 ->
  {'Ip6Address', Addr}.

return({ok, Result}) -> Result;
return({error, Reason}) -> error(Reason).
