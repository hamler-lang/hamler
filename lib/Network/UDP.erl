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

-export([ open/1
        , openWith/2
        , recv/2
        , recvTimeout/3
        , send/4
        , sendTo/4
        , close/1
        , controllingProcess/2
        ]).

open(Port) ->
  ?IO(returnVal(gen_udp:open(Port))).

openWith(Port, Options) ->
  ?IO(returnVal(gen_udp:open(Port, parseOpts(Options)))).

recv(Socket, Length) ->
  ?IO(wrap(returnVal(gen_udp:recv(Socket, Length)))).

recvTimeout(Socket, Length, Timeout) ->
  ?IO(wrap(returnVal(gen_udp:recv(Socket, Length, unwrap(Timeout))))).

send(Socket, Hostname, PortNumber, Packet) ->
  ?IO(return(gen_udp:send(Socket, Hostname, PortNumber, Packet))).

sendTo(Socket, IpAddress, PortNumber, Packet) ->
  ?IO(return(gen_udp:send(Socket, unwrap(IpAddress), PortNumber, Packet))).

close(Socket) ->
  ?IO(return(gen_udp:close(Socket))).

controllingProcess(Socket, Pid) ->
  ?IO(return(gen_udp:controlling_process(Socket, Pid))).

returnVal({ok, Socket}) -> Socket;
returnVal({error, Reason}) -> error(Reason).

return(ok) -> ok;
return({error, Reason}) -> error(Reason).

parseOpts(Opts) -> parseOpts(Opts, [binary]).

parseOpts([{'Active', B}|Opts], Acc) ->
  parseOpts(Opts, [{active, B}|Acc]);
parseOpts([{'ActiveN', N}|Opts], Acc) ->
  parseOpts(Opts, [{active, N}|Acc]);
parseOpts([{'AddMembership', Addr1, Addr2}|Opts], Acc) ->
  parseOpts(Opts, [{add_membership, unwrap(Addr1), unwrap(Addr2)}|Acc]);
parseOpts([{'Broadcast', B}|Opts], Acc) ->
  parseOpts(Opts, [{broadcast, B}|Acc]);
parseOpts([{'Buffer', N}|Opts], Acc) ->
  parseOpts(Opts, [{buffer, N}|Acc]);
parseOpts([{'Deliver', A}|Opts], Acc) ->
  parseOpts(Opts, [{deliver, A}|Acc]);
parseOpts([{'Dontroute', B}|Opts], Acc) ->
  parseOpts(Opts, [{dontroute, B}|Acc]);
parseOpts([{'DropMembership', Addr1, Addr2}|Opts], Acc) ->
  parseOpts(Opts, [{drop_membership, unwrap(Addr1), unwrap(Addr2)}|Acc]);
parseOpts([{'Header', N}|Opts], Acc) ->
  parseOpts(Opts, [{header, N}|Acc]);
parseOpts([{'HighMsgqWatermark', N}|Opts], Acc) ->
  parseOpts(Opts, [{high_msgq_watermark, N}|Acc]);
parseOpts([{'LowMsgqWatermark', N}|Opts], Acc) ->
  parseOpts(Opts, [{low_msgq_watermark, N}|Acc]);
parseOpts([{'MulticastIf', Addr}|Opts], Acc) ->
  parseOpts(Opts, [{multicast_if, unwrap(Addr)}|Acc]);
parseOpts([{'MulticastTTL', N}|Opts], Acc) ->
  parseOpts(Opts, [{multicast_ttl, N}|Acc]);
parseOpts([{'Priority', N}|Opts], Acc) ->
  parseOpts(Opts, [{priority, N}|Acc]);
parseOpts([{'Raw', N1, N2, Bin}|Opts], Acc) ->
  parseOpts(Opts, [{raw, N1, N2, Bin}|Acc]);
parseOpts([{'ReadPackets', N}|Opts], Acc) ->
  parseOpts(Opts, [{read_packets, N}|Acc]);
parseOpts([{'Recbuf', N}|Opts], Acc) ->
  parseOpts(Opts, [{recbuf, N}|Acc]);
parseOpts([{'ReuseAddr', B}|Opts], Acc) ->
  parseOpts(Opts, [{reuseaddr, B}|Acc]);
parseOpts([{'Sndbuf', N}|Opts], Acc) ->
  parseOpts(Opts, [{sndbuf, N}|Acc]);
parseOpts([{'ToS', N}|Opts], Acc) ->
  parseOpts(Opts, [{tos, N}|Acc]);
parseOpts([{'Tclass', N}|Opts], Acc) ->
  parseOpts(Opts, [{tclass, N}|Acc]);
parseOpts([{'TTL', N}|Opts], Acc) ->
  parseOpts(Opts, [{ttl, N}|Acc]);
parseOpts([{'RecvToS', B}|Opts], Acc) ->
  parseOpts(Opts, [{recvtos, B}|Acc]);
parseOpts([{'RecvTclass', B}|Opts], Acc) ->
  parseOpts(Opts, [{recvtclass, B}|Acc]);
parseOpts([{'RecvTTL', B}|Opts], Acc) ->
  parseOpts(Opts, [{recvttl, B}|Acc]);
parseOpts([{'Ipv6only', B}|Opts], Acc) ->
  parseOpts(Opts, [{ipv6_v6only, B}|Acc]);
parseOpts([], Acc) -> Acc.

unwrap({'Ip4Address', Addr}) -> Addr;
unwrap({'Ip6Address', Addr}) -> Addr;
unwrap({'Infinity'}) -> infinity;
unwrap({'Timeout', I}) -> I.

wrap({Address, Port, Packet}) ->
  {wrapAddr(Address), Port, Packet}.
wrapAddr(Addr) when size(Addr) == 4 ->
  {'Ip4Address', Addr};
wrapAddr(Addr) when size(Addr) == 6 ->
  {'Ip6Address', Addr}.
