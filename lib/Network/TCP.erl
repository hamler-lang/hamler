%%---------------------------------------------------------------------------
%% |
%% Module      :  TCP
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
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

-export([ listen/2
        , accept/1
        , acceptTimeout/2
        , connect/3
        , connectTimeout/4
        , recv/2
        , recvTimeout/3
        , send/2
        , shutdown/2
        , close/1
        , controllingProcess/2
        ]).

listen(Port, Options) ->
  ?IO(return(gen_tcp:listen(Port, Options))).

accept(LSocket) ->
  ?IO(return(gen_tcp:accept(LSocket))).

acceptTimeout(LSocket, Timeout) ->
  ?IO(return(gen_tcp:accept(LSocket, Timeout))).

connect(Address, Port, Options) ->
  ?IO(return(gen_tcp:connect(toErl(Address), Port, Options))).

connectTimeout(Address, Port, Options, Timeout) ->
  ?IO(return(gen_tcp:connect(toErl(Address), Port, Options, Timeout))).

recv(Socket, Length) ->
  ?IO(return(gen_tcp:recv(Socket, Length))).

recvTimeout(Socket, Length, Timeout) ->
  ?IO(return(gen_tcp:recv(Socket, Length, Timeout))).

send(Socket, Packet) ->
  ?IO(return(gen_tcp:send(Socket, Packet))).

shutdown(Socket, How) ->
  ?IO(return(gen_tcp:shutdown(Socket, How))).

close(Socket) ->
  ?IO(return(gen_tcp:close(Socket))).

controllingProcess(Socket, Pid) ->
  ?IO(return(gen_udp:controlling_process(Socket, Pid))).

toErl({'Ip4Address', A, B, C, D}) -> {A, B, C, D};
toErl({'Ip6Address', A, B, C, D, E, F, G, H}) ->
  {A, B, C, D, E, F, G, H}.

return(ok) -> ok;
return({ok, Result}) -> Result;
return({error, Reason}) -> error(Reason).
