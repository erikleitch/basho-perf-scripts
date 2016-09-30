-module(ts_setup_sonotest).
-behavior(riak_test).
-export([confirm/0]).

-include_lib("profiler/include/profiler.hrl").

confirm() ->
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    ts_test_utils:set_max_quanta(1000),
    ts_test_utils:setup_ts_cluster(multiple, 1, geo, 1),
    riak_prof_tests:putSequentialTsData(100, 1000),
    riak_prof_tests:geoQuery(10000),
    pass.
