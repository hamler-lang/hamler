-ifndef(DATA_TIME_HRL).
-define(DATA_TIME_HRL, true).

time_unit({'Second'}) -> second;
time_unit({'Millisecond'}) -> millisecond;
time_unit({'Microsecond'}) -> microsecond;
time_unit({'Nanosecond'}) -> nanosecond;
time_unit({'Native'}) -> native;
time_unit({'PerfCounter'}) -> perf_counter.

-endif.
