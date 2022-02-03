%%---------------------------------------------------------------------------
%% |
%% Module      :  TCP
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The TCP FFI module.
%%
%%---------------------------------------------------------------------------
-module('TCP').

-include("../Foreign.hrl").

-export([ accept/1
        , acceptTimeout/2
        , connect/3
        , connectTimeout/4
        , listen/2
        , recv/3
        , recvTimeout/4
        , send/3
        , shutdown/2
        ]).

-define(HMod, 'Network.TCP').

accept(LSocket) ->
    ?IO(return(gen_tcp:accept(LSocket))).

acceptTimeout(LSocket, Timeout) ->
    ?IO(return(gen_tcp:accept(LSocket, Timeout))).

connect(Address, Port, Options) ->
    Opts = ?TOFFIs(?HMod, isffiTcpOptionTerm, Options),
    ?IO(return(gen_tcp:connect(unwrap(Address), Port, Opts))).

connectTimeout(Address, Port, Options, Timeout) ->
    Opts = ?TOFFIs(?HMod, isffiTcpOptionTerm, Options),
    ?IO(return(gen_tcp:connect(unwrap(Address), Port, Opts, Timeout))).

listen(Port, Options) ->
    Opts = ?TOFFIs(?HMod, isffiListenOptionTerm, Options),
    ?IO(return(gen_tcp:listen(Port, Opts))).

recv(_, Socket, Length) ->
    ?IO(return(gen_tcp:recv(Socket, Length))).

recvTimeout(_, Socket, Length, Timeout) ->
     ?IO(return(gen_tcp:recv(Socket, Length, unwrap(Timeout)))).

send(_, Socket, Packet) ->
    ?IO(return(gen_tcp:send(Socket, Packet))).

shutdown(Socket, Method) ->
    How = ?TOFFI(?HMod, isffiShutdownMethodAtom, Method),
    ?IO(return(gen_tcp:shutdown(Socket, How))).

unwrap({'Ip4Address', Addr}) -> Addr;
unwrap({'Ip6Address', Addr}) -> Addr;
unwrap({'Infinity'}) -> infinity;
unwrap({'Timeout', I}) -> I.

return(ok) -> ok;
return({ok, Result}) -> Result;
return({error, Reason}) -> error(Reason).



