-module(ts_setup_kv_nval1_w1c).
-behavior(riak_test).
-export([confirm/0]).

-include_lib("profiler/include/profiler.hrl").

confirm() ->
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    ts_test_utils:setup_kv_cluster(single, 1, "TestBucketType", true),
    pass.
