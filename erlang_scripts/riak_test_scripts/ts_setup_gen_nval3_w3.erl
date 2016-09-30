-module(ts_setup_gen_nval3_w3).
-behavior(riak_test).
-export([confirm/0]).

-include_lib("profiler/include/profiler.hrl").

confirm() ->
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    ColList = [1, 5, 10, 20, 50],
    ts_test_utils:setup_ts_gen_cluster(multiple, 3, 3, ColList),
    pass.
