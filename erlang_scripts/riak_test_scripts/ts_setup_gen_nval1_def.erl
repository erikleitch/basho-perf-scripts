-module(ts_setup_gen_nval1_def).
-behavior(riak_test).
-export([confirm/0]).

-include_lib("profiler/include/profiler.hrl").

confirm() ->
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    ColList = [1, 5, 10, 20, 50],
    ts_test_utils:setup_ts_gen_cluster(single, 1, ColList),
    pass.
