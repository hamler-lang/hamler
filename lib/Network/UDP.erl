%%---------------------------------------------------------------------------
%% |
%% Module      :  UDP
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The UDP FFI module.
%%
%%---------------------------------------------------------------------------
-module('UDP').

-include("../Foreign.hrl").

-export([ controllingProcess/2
        , open/1
        , openWith/2
        , recv/2
        , recvTimeout/3
        , send/4
        ]).

controllingProcess(Socket, Pid) ->
  ?IO(case gen_udp:controllingProcess(Socket, Pid) of
        ok -> ok;
        {error, Reason} -> error(Reason)
      end).

open(Port) ->
  ?IO(case gen_udp:open(Port) of
        {ok, Socket} -> Socket;
        {error, Reason} -> error(Reason)
      end).

openWith(Port, Opts) ->
  ?IO(case gen_udp:open(Port, Opts) of
        {ok, Socket} -> Socket;
        {error, Reason} -> error(Reason)
      end).

recv(Socket, Length) ->
  ?IO(case gen_udp:recv(Socket, Length) of
        {ok, {Address, Port, Packet}} ->
          {ipToHM(Address), Port, Packet};
        {error, Reason} -> error(Reason)
      end).

recvTimeout(Socket, Length, Timeout) ->
  ?IO(case gen_udp:recv(Socket, Length, Timeout) of
        {ok, {Address, Port, Packet}} ->
          {ipToHM(Address), Port, Packet};
        {error, Reason} -> error(Reason)
      end).

send(Socket, IpAddress, PortNumber, Packet) ->
  ?IO(case gen_udp:send(Socket, ipToErl(IpAddress), PortNumber, Packet) of
        ok -> ok;
        {error, Reason} -> error(Reason)
      end).

ipToErl({'Ip4Address', A, B, C, D}) -> {A, B, C, D};
ipToErl({'Ip6Address', A, B, C, D, E, F, G, H}) -> {A, B, C, D, E, F, G, H}.

ipToHM({A, B, C, D}) -> {'Ip4Address', A, B, C, D};
ipToHM({A, B, C, D, E, F, G, H}) -> {'Ip6Address', A, B, C, D, E, F, G, H}.
