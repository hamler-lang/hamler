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

-include("../Foreign.hrl").

-compile(no_auto_import).

-export([ selfPid/0
        , spawn/2
        , send/2
        , 'receive'/0
        , receiveAfter/1
        , register/2
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
selfPid() -> ?IO(erlang:self()).

spawn(Fun, Arg) ->
  ?IO(erlang:spawn(fun() -> Fun(Arg) end)).

-spec(send(pid(), term()) -> term()).
send(Pid, Msg) -> ?IO(erlang:send(Pid, Msg)).

-spec('receive'() -> term()).
'receive'() -> ?IO(receive X -> X end).

%% TODO: Fixme later:(
-spec(receiveAfter(integer()) -> term()).
receiveAfter(Timeout) ->
  ?IO(receive X -> X after Timeout -> ok end).

register(Name, Pid) ->
  true = erlang:register(Name, Pid), ok.

-spec('monitor'(pid()) -> reference()).
'monitor'(Pid) ->
  ?IO(erlang:monitor(process, Pid)).

eraseAll() -> ?IO(erlang:erase()).

erase(Key) ->
  ?IO(case erlang:erase(Key) of
        undefined -> {'Nothing'};
        Val -> {'Just', Val}
      end).

garbageCollectProcWith(Pid, Options) ->
  ?IO(erlang:garbage_collect(Pid, parseGcOpts(Options, []))).

parseGcOpts([{'GcMajor'}|Opts], Acc) ->
  parseGcOpts(Opts, [{type, major}|Acc]);
parseGcOpts([{'GcMinor'}|Opts], Acc) ->
  parseGcOpts(Opts, [{type, minor}|Acc]);
parseGcOpts([{'GcAsync', RequestId}|Opts], Acc) ->
  parseGcOpts(Opts, [{async, RequestId}|Acc]);
parseGcOpts([], Acc) -> Acc.

whereis(Name) ->
  ?IO(case erlang:whereis(Name) of
        undefined -> {'Nothing'};
        Pid -> {'Just', Pid}
      end).

kill(Pid) -> ?IO(erlang:exit(Pid, kill)).

-spec(trapExit(boolean()) -> boolean()).
trapExit(Flag) ->
  ?IO(erlang:process_flag(trap_exit, Flag)).

processFlag({'TrapExit', Bool}) ->
  ?IO({'TrapExit', erlang:process_flag(trap_exit, Bool)});
processFlag({'ErrorHandler', Module}) ->
  ?IO({'ErrorHandler', erlang:process_flag(error_handler, Module)});
processFlag({'MinHeapSize', Size}) ->
  ?IO({'MinHeapSize', erlang:process_flag(min_heap_size, Size)});
processFlag({'MinBinVheapSize', Size}) ->
  ?IO({'MinBinVheapSize', erlang:process_flag(min_bin_vheap_size, Size)});
processFlag({'MaxHeapSize', Size}) ->
  ?IO({'MaxHeapSize', erlang:process_flag(max_heap_size, Size)});
processFlag({'MsgQueueData', MQD}) ->
  ?IO({'MsgQueueData', mqd(erlang:process_flag(message_queue_data, mqd(MQD)))});
processFlag({'Priority', Level}) ->
  ?IO({'Priority', level(erlang:process_flag(priority, level(Level)))});
processFlag({'SaveCalls', N}) ->
  ?IO({'SaveCalls', erlang:process_flag(save_calls, N)});
processFlag({'Sensitive', Bool}) ->
  ?IO({'Sensitive', erlang:process_flag(sensitive, Bool)}).

processInfo(Pid) -> ?IO(erlang:process_info(Pid)).

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

