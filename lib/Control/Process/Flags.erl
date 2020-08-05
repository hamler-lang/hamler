%%---------------------------------------------------------------------------
%% |
%% Module      :  Flags
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Process Flag FFI module.
%%
%%---------------------------------------------------------------------------
-module('Flags').

-include("../../Foreign.hrl").

-export([processFlag/1, trapExit/1]).

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

-spec(trapExit(boolean()) -> boolean()).
trapExit(Flag) -> ?IO(erlang:process_flag(trap_exit, Flag)).

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
