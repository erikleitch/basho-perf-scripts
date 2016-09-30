-module(ts_setup_standard_node3_nval1).
-behavior(riak_test).
-export([confirm/0]).

-include_lib("profiler/include/profiler.hrl").

confirm() ->
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    ts_test_utils:setup_ts_cluster(multiple, 1, geo),
    pass.
