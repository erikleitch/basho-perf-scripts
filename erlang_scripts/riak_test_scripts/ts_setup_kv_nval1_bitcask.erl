-module(ts_setup_kv_nval1_bitcask).
-behavior(riak_test).
-export([confirm/0]).

-include_lib("profiler/include/profiler.hrl").

confirm() ->
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    ts_test_utils:setup_kv_cluster(single_bitcask_nif, 1, "TestBucketType", false),

    pass.
