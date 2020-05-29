%%---------------------------------------------------------------------------
%% |
%% Module      :  Socket
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Socket FFI Module.
%%
%%---------------------------------------------------------------------------
-module('Socket').

-export([ accept/1
        , acceptTimeout/2
        , bind/2
        , bindAny/1
        , bindBroadcast/1
        , bindLoopback/1
        , cancel/2
        , close/1
        , connect/2
        , connectTimeout/3
        , info/1
        , listen/1
        , listenWithBacklog/2
        , numberOf/0
        , open/2
        , recv/1
        , recvWithLen/2
        , recvWithFlags/3
        , recvWithTimeout/3
        , recvFrom/1
        , send/2
        , sendTo/3
        , shutdown/2
        ]).

-export([ getOpt/3
        , getPeerName/1
        , getSockName/1
        ]).

-type(sockAddr() :: tuple()).

-spec(accept(socket:socket()) -> socket:socket()).
accept(LSocket) ->
    return(socket:accept(LSocket)).

-spec(acceptTimeout(socket:socket(), timeout()) -> socket:socket()).
acceptTimeout(LSocket, Timeout) ->
    return(socket:accept(LSocket, Timeout)).

-spec(bind(socket:socket(), sockAddr()) -> socket:port_number()).
bind(Socket, SockAddr) ->
    return(socket:bind(Socket, destruct(SockAddr))).

-spec(bindAny(socket:socket()) -> socket:port_number()).
bindAny(Socket) ->
    return(socket:bind(Socket, any)).

-spec(bindBroadcast(socket:socket()) -> socket:port_number()).
bindBroadcast(Socket) ->
    return(socket:bind(Socket, broadcast)).

-spec(bindLoopback(socket:socket()) -> socket:port_number()).
bindLoopback(Socket) ->
    return(socket:bind(Socket, loopback)).

-spec(cancel(socket:socket(), socket:select_info()) -> ok).
cancel(Socket, SelectInfo) ->
    return(socket:cancel(Socket, destruct(SelectInfo))).

-spec(close(socket:socket()) -> ok).
close(Socket) ->
    return(socket:close(Socket)).

-spec(connect(socket:socket(), sockAddr()) -> ok).
connect(Socket, SockAddr) ->
    return(socket:connect(Socket, destruct(SockAddr))).

-spec(connectTimeout(socket:socket(), sockAddr(), timeout()) -> ok).
connectTimeout(Socket, SockAddr, Timeout) ->
    return(socket:connect(Socket, destruct(SockAddr), Timeout)).

-spec(getOpt(socket:socket(), string(), string()) -> term()).
getOpt(Socket, Level, Key) ->
    return(socket:getopt(Socket, atom(Level), atom(Key))).

-spec(getPeerName(socket:socket()) -> socket:sockaddr()).
getPeerName(Socket) ->
    return(socket:peername(Socket), fun construct/1).

-spec(getSockName(socket:socket()) -> socket:sockaddr()).
getSockName(Socket) ->
    return(socket:sockname(Socket), fun construct/1).

-spec(info(socket:socket()) -> socket:socket_info()).
info(Socket) -> socket:info(Socket).

-spec(listen(socket:socket()) -> ok).
listen(Socket) -> return(socket:listen(Socket)).

-spec(listenWithBacklog(socket:socket(), pos_integer()) -> ok).
listenWithBacklog(Socket, Backlog) ->
    return(socket:listen(Socket, Backlog)).

-spec(numberOf() -> integer()).
numberOf() -> socket:number_of().

-spec(open(socket:domain(), socket:type()) -> socket:socket()).
open(Domain, Type) -> return(socket:open(destruct(Domain), destruct(Type))).

recv(Socket) -> return(socket:recv(Socket)).

recvWithLen(Socket, Length) ->
    return(socket:recv(Socket, Length)).

recvWithFlags(Socket, Length, Flags) ->
    return(socket:recv(Socket, Length, [destruct(Flag) || Flag <- Flags])).

recvWithTimeout(Socket, Length, Timeout) ->
    return(socket:recv(Socket, Length, Timeout)).

recvFrom(Socket) ->
    Construct = fun({undefined, Data}) ->
                        {'Nothing', Data};
                   ({Source, Data}) ->
                        {{'Just', construct(Source)}, Data}
                end,
    return(socket:recvFrom(Socket), Construct).

-spec(send(socket:socket(), binary()) -> ok).
send(Socket, Data) ->
    return(socket:send(Socket, Data)).

-spec(sendTo(socket:socket(), binary(), sockAddr()) -> ok).
sendTo(Socket, Data, Dest) ->
    return(socket:sendto(Socket, Data, destruct(Dest))).

-spec(shutdown(socket:socket(), string()) -> ok).
shutdown(Socket, How) ->
    return(socket:shutdown(Socket, atom(How))).

%%---------------------------------------------------------------------------
%% Erlang -> Hamler
%%---------------------------------------------------------------------------

construct(#{family := inet,
            port   := Port,
            addr   := Addr
           }) ->
    {'SockAddrInet', Port, ip4Addr(Addr)};
construct(#{family   := inet6,
            port     := Port,
            addr     := Addr,
            flowinfo := FlowInfo,
            scope_id := ScopeId
           }) ->
    {'SockAddrInet6', Port, ip6Addr(Addr), FlowInfo, ScopeId};
construct(#{family := local,
            path   := Path
           }) when is_binary(Path) ->
    {'SockAddrUnix', binary_to_list(Path)};
construct(#{family := local,
            path   := Path
           }) ->
    {'SockAddrUnix', Path};
construct(#{family   := packet,
            protocol := PhyProto,
            ifindex  := IfIndex,
            pkttype  := PktType,
            hatype   := HaType,
            addr     := HaAddr
           }) ->
    {'SockAddrLL', PhyProto, IfIndex, PktType, HaType, HaAddr}.

%%---------------------------------------------------------------------------
%% Hamler -> Erlang
%%---------------------------------------------------------------------------

destruct({'Ip4Address', A, B, C, D}) ->
    {A, B, C, D};
destruct({'Ip6Address', A, B, C, D, E, F, G, H}) ->
    {A, B, C, D, E, F, G, H};

destruct({'SockAddr', Port, Addr}) ->
    #{family => inet,
      port   => Port,
      addr   => destruct(Addr)
     };
destruct({'SockAddrInet6', Port, Addr, FlowInfo, ScopeId}) ->
    #{family   => inet6,
      port     => Port,
      addr     => destruct(Addr),
      flowinfo => FlowInfo,
      scope_id => ScopeId
     };
destruct({'SockAddrUnix', Path}) ->
    #{family => local,
      path   => Path
     };
destruct({'SockAddrLL', PhyProto, IfIndex, PktType, HaType, HaAddr}) ->
    #{family   => packet,
      protocol => PhyProto,
      ifindex  => IfIndex,
      pkttype  => PktType,
      hatype   => HaType,
      addr     => HaAddr
     };
destruct({'SelectInfo', Tag, Ref}) ->
    {select_info, Tag, Ref};

destruct({'Local'}) -> local;
destruct({'Inet'})  -> inet;
destruct({'Inet6'}) -> inet6;

destruct({'Stream'}) -> stream;
destruct({'Dgram'})  -> dgram;
destruct({'Raw'})    -> raw;
destruct({'Rdm'})    -> rdm;
destruct({'SeqPacket'}) -> seqpacket;

destruct({'CmsgCloexec'}) -> cmsg_cloexec;
destruct({'ErrQueue'}) -> errqueue;
destruct({'Oob'}) -> oob;
destruct({'Peek'}) -> peek;
destruct({'Trunc'}) -> trunc.

ip4Addr(any) ->
    ip4Addr({0, 0, 0, 0});
ip4Addr(broadcast) ->
    ip4Addr({255, 255, 255, 255});
ip4Addr(loopback) ->
    ip4Addr({127, 0, 0, 1});
ip4Addr({A, B, C, D}) ->
    {'Ip4Address', A, B, C, D}.

ip6Addr(any) ->
    ip6Addr({0, 0, 0, 0, 0, 0, 0, 0});
ip6Addr(loopback) ->
    ip4Addr({0, 0, 0, 0, 0, 0, 0, 1});
ip6Addr({A, B, C, D, E, F, G, H}) ->
    {'Ip6Address', A, B, C, D, E, F, G, H}.

%%---------------------------------------------------------------------------
%% Utility functions
%%---------------------------------------------------------------------------

-compile({inline, [return/1, return/2]}).
return(ok) -> ok;
return({ok, Result}) -> Result;
return({error, Reason}) -> error(Reason).

return({ok, Result}, Construct) -> Construct(Result);
return({error, Reason}, _) -> error(Reason).

-compile({inline, [atom/1]}).
atom(S) -> list_to_existing_atom(S).
