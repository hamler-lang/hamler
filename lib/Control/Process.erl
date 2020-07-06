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
%% The Process FFI module.
%%
%%---------------------------------------------------------------------------
-module('Process').

-compile(no_auto_import).

-export([ selfPid/0
        , spawn/2
        , send/2
        , 'receive'/0
        , receiveAfter/1
        , 'monitor'/1
        , erase/1
        , eraseAll/0
        , garbageCollectProcWith/2
        , whereis/1
        , kill/1
        , trapExit/1
        , processFlag/1
        , processInfo/1
        ]).

-spec(selfPid() -> pid()).
selfPid() -> erlang:self().

spawn(Fun, Arg) -> erlang:spawn(fun() -> Fun(Arg) end).

-spec(send(pid(), term()) -> term()).
send(Pid, Msg) -> erlang:send(Pid, Msg).

-spec('receive'() -> term()).
'receive'() -> receive X -> X end.

%% TODO: Fixme later:(
-spec(receiveAfter(integer()) -> term()).
receiveAfter(Timeout) ->
    receive X -> X after Timeout -> ok end.

-spec('monitor'(pid()) -> reference()).
'monitor'(Pid) -> erlang:monitor(process, Pid).

eraseAll() -> erlang:erase().

erase(Key) ->
  case erlang:erase(Key) of
    undefined -> {'Nothing'};
    Val -> {'Just', Val}
  end.

garbageCollectProcWith(Pid, Options) ->
  erlang:garbage_collect(Pid, parseGcOpts(Options, [])).

parseGcOpts([{'GcMajor'}|Opts], Acc) ->
  parseGcOpts(Opts, [{type, major}|Acc]);
parseGcOpts([{'GcMinor'}|Opts], Acc) ->
  parseGcOpts(Opts, [{type, minor}|Acc]);
parseGcOpts([{'GcAsync', RequestId}|Opts], Acc) ->
  parseGcOpts(Opts, [{async, RequestId}|Acc]);
parseGcOpts([], Acc) -> Acc.

whereis(Name) ->
  case erlang:whereis(Name) of
    undefined -> {'Nothing'};
    Pid -> {'Just', Pid}
  end.

kill(Pid) -> erlang:exit(Pid, kill).

-spec(trapExit(boolean()) -> boolean()).
trapExit(Flag) -> erlang:process_flag(trap_exit, Flag).

processFlag({'TrapExit', Bool}) ->
  {'TrapExit', erlang:process_flag(trap_exit, Bool)};
processFlag({'ErrorHandler', Module}) ->
  {'ErrorHandler', erlang:process_flag(error_handler, Module)};
processFlag({'MinHeapSize', Size}) ->
  {'MinHeapSize', erlang:process_flag(min_heap_size, Size)};
processFlag({'MinBinVheapSize', Size}) ->
  {'MinBinVheapSize', erlang:process_flag(min_bin_vheap_size, Size)};
processFlag({'MaxHeapSize', Size}) ->
  {'MaxHeapSize', erlang:process_flag(max_heap_size, Size)};
processFlag({'MsgQueueData', MQD}) ->
  {'MsgQueueData', mqd(erlang:process_flag(message_queue_data, mqd(MQD)))};
processFlag({'Priority', Level}) ->
  {'Priority', level(erlang:process_flag(priority, level(Level)))};
processFlag({'SaveCalls', N}) ->
  {'SaveCalls', erlang:process_flag(save_calls, N)};
processFlag({'Sensitive', Bool}) ->
  {'Sensitive', erlang:process_flag(sensitive, Bool)}.

processInfo(Pid) -> erlang:process_info(Pid).

mqd({'OffHeap'}) -> off_heap;
mqd({'OnHeap'})  -> on_heap;
mqd(off_heap)    -> {'OffHeap'};
mqd(on_heap)     -> {'OnHeap'}.

level({'Low'})    -> low;
level({'Normal'}) -> normal;
level({'High'})   -> high;
level({'Max'})    -> max;
level(low)    -> {'Low'};
level(normal) -> {'Normal'};
level(high)   -> {'High'};
level(max)    -> {'Max'}.

