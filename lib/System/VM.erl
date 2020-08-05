%%---------------------------------------------------------------------------
%% |
%% Module      :  VM
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Erlang VM FFI module.
%%
%%---------------------------------------------------------------------------
-module('VM').

-include("../Foreign.hrl").

-export([ memory/0
        , info/1
        , stats/1
        ]).

-import(erlang, [system_info/1]).
-import('Maybe', [maybe/1]).

memory() ->
  ?IO(maps:from_list(erlang:memory())).

%%---------------------------------------------------------------------------
%% | VM flags
%%---------------------------------------------------------------------------

info(allocated_areas) ->
  ?IO(maps:from_list(fixAtomSpace(system_info(allocated_areas))));
info(cpu_topology) ->
  ?IO(maybe(system_info(cpu_topology)));
info(logical_processors) ->
  ?IO(fixUnknown(system_info(logical_processors)));
info(fullsweep_after) ->
  ?IO(snd(system_info(fullsweep_after)));
info(garbage_collection) ->
  ?IO(maps:from_list(system_info(garbage_collection)));
info(max_heap_size) ->
  ?IO(fixMaxHeap(system_info(max_heap_size)));
info(min_heap_size) ->
  ?IO(snd(system_info(min_heap_size)));
info(scheduler_bindings) ->
  ?IO(tuple_to_list(system_info(scheduler_bindings)));
info(delayed_node_table_gc) ->
  ?IO(timeout(system_info(delayed_node_table_gc)));
info(modified_timing_level) ->
  ?IO(maybe(system_info(modified_timing_level)));
info(wordsize_intenal) ->
  ?IO(system_info({wordsize, intenal}));
info(wordsize_external) ->
  ?IO(system_info({wordsize, external}));
info(Item) -> ?IO(system_info(Item)).

%%---------------------------------------------------------------------------
%% | VM stats
%%---------------------------------------------------------------------------

stats(context_switches) ->
  ?IO(fst(statistics(context_switches)));
stats(number_of_gcs) ->
  ?IO(fst(statistics(garbage_collection)));
stats(words_reclaimed) ->
  ?IO(snd(statistics(garbage_collection)));
stats(io_input) ->
  ?IO(snd(fst(statistics(io))));
stats(io_output) ->
  ?IO(snd(snd(statistics(io))));
stats(io_stats) ->
  ?IO(begin
        {Input, Output} = statistics(io),
        {snd(Input), snd(Output)}
      end);
stats(microstate_accounting) ->
  ?IO(case statistics(microstate_accounting) of
        undefined -> ?Nothing;
        Stats ->
          ?Just([msaccThread(Stat) || Stat <- Stats])
      end);
stats(scheduler_wall_time) ->
  ?IO(maybe(statistics(scheduler_wall_time)));
stats(scheduler_wall_time_all) ->
  ?IO(maybe(statistics(scheduler_wall_time_all)));
stats(Item) -> ?IO(statistics(Item)).

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

fixAtomSpace(Areas) -> fixAtomSpace(Areas, []).
fixAtomSpace([{atom_space, Alloced, Used}|Areas], Acc) ->
  fixAtomSpace(Areas, [{atom_alloced, Alloced}, {atom_used, Used}|Acc]);
fixAtomSpace([H|T], Acc) -> fixAtomSpace(T, [H|Acc]);
fixAtomSpace([], Acc) -> Acc.

fixUnknown(unknown) -> 0;
fixUnknown(I) -> I.

fixMaxHeap(I) when is_integer(I) ->
  #{size => I, kill => false, error_logger => false};
fixMaxHeap(M) when is_map(M) -> M.

msaccThread(#{type := Type, id := Id, counters := Counters}) ->
  #{thread => {Type, Id}, counters => Counters}.

timeout(infinity) -> {'Infinity'};
timeout(I) -> {'Timeout', I}.

fst(T) -> element(1, T).
snd(T) -> element(2, T).
