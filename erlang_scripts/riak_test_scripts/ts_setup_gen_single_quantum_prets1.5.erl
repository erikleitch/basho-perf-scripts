-module(ts_setup_gen_single_quantum_nomd).
-behavior(riak_test).
-export([confirm/0]).

-include_lib("profiler/include/profiler.hrl").

confirm() ->
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    ts_test_utils:set_max_quanta(1000),
    ts_test_utils:set_max_concurrent(1000),
%%    ts_test_utils:set_max_query_size(1000000000),

    ColList = [1, 10, 100, 200],

    %% 10,0000 seconds will cause up to 10,000 1-s records to be written to a
    %% single partition (since timestamps are just incremented)

    ts_test_utils:setup_ts_gen_cluster(multiple, 1, ColList, 10000, "s"),

    pass.
