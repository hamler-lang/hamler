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

-export([ accept/1
        , acceptTimeout/2
        , connect/3
        , connectTimeout/4
        , listen/2
        , recv/2
        , recvTimeout/3
        , send/2
        , shutdown/2
        ]).

accept(LSocket) ->
  return(gen_tcp:accept(LSocket)).

acceptTimeout(LSocket, Timeout) ->
  return(gen_tcp:accept(LSocket, Timeout)).

connect(Address, Port, Options) ->
  return(gen_tcp:connect(toErl(Address), Port, Options)).

connectTimeout(Address, Port, Options, Timeout) ->
  return(gen_tcp:connect(toErl(Address), Port, Options, Timeout)).

listen(Port, Options) ->
  return(gen_tcp:listen(Port, Options)).

recv(Socket, Length) ->
  return(gen_tcp:recv(Socket, Length)).

recvTimeout(Socket, Length, Timeout) ->
  return(gen_tcp:recv(Socket, Length, Timeout)).

send(Socket, Packet) ->
  return(gen_tcp:send(Socket, Packet)).

shutdown(Socket, How) ->
  return(gen_tcp:shutdown(Socket, How)).

toErl({'Ip4Address', A, B, C, D}) -> {A, B, C, D};
toErl({'Ip6Address', A, B, C, D, E, F, G, H}) -> {A, B, C, D, E, F, G, H}.

return(ok) -> ok;
return({ok, Result}) -> Result;
return({error, Reason}) -> error(Reason).
