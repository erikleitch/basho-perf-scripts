-module(ts_setup_intellicore_h_multi).
-behavior(riak_test).
-export([confirm/0]).

-include_lib("profiler/include/profiler.hrl").

confirm() ->
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    ts_test_utils:set_max_quanta(1000),
    ts_test_utils:setup_ts_cluster(intellicore_multi, 1, intellicore, "h"),

    riak_prof_tests:intellicoreTest(),
    riak_prof_tests:iq(laps, laps),

    pass.
