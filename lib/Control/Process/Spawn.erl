%%---------------------------------------------------------------------------
%% |
%% Module      :  Spwan
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Spwan FFI module.
%%
%%---------------------------------------------------------------------------
-module('Spawn').

-include("../../Foreign.hrl").

-export([ spawnWith/2
        , spawnAtWith/3
        , spawnLinkWith/2
        , spawnLinkAtWith/3
        , spawnMonitorWith/2
        , spawnMonitorAtWith/3
        ]).

spawnWith(Fun, Options) ->
  ?IO(erlang:spawn_opt(Fun, parseOpts(Options))).

spawnAtWith(Node, Fun, Options) ->
  ?IO(erlang:spawn_opt(Node, Fun, parseOpts(Options))).

spawnLinkWith(Fun, Options) ->
  ?IO(erlang:spawn_opt(Fun, [link|parseOpts(Options)])).

spawnLinkAtWith(Node, Fun, Options) ->
  ?IO(erlang:spawn_opt(Node, Fun, [link|parseOpts(Options)])).

spawnMonitorWith(Fun, Options) ->
  ?IO(erlang:spawn_opt(Fun, [monitor|parseOpts(Options)])).

spawnMonitorAtWith(Node, Fun, Options) ->
  ?IO(erlang:spawn_opt(Node, Fun, [monitor|parseOpts(Options)])).

%%---------------------------------------------------------------------------
%% | Parse options
%%---------------------------------------------------------------------------

parseOpts(Opts) -> [mappingOpt(Opt) || Opt <- Opts].

mappingOpt({'Priority', Level}) ->
  {priority, unwrap(Level)};
mappingOpt({'FullsweepAfter', N}) ->
  {fullsweep_after, N};
mappingOpt({'MinHeapSize', Size}) ->
  {min_heap_size, Size};
mappingOpt({'MinBinVheapSize', Size}) ->
  {min_bin_vheap_size, Size};
mappingOpt({'MaxHeapSize', Size}) ->
  {max_heap_size, Size};
mappingOpt({'MessageQueueData', MQD}) ->
  {message_queue_data, unwrap(MQD)}.

unwrap('Low') -> low;
unwrap('Normal') -> normal;
unwrap('High') -> high;
unwrap('Max') -> max;

unwrap('OnHeap') -> on_heap;
unwrap('OffHeap') -> off_heap.

