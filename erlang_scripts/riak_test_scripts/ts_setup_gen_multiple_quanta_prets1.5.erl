-module(ts_setup_gen_multiple_quanta_nomd).
-behavior(riak_test).
-export([confirm/0]).

-include_lib("profiler/include/profiler.hrl").

confirm() ->
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    ts_test_utils:set_max_quanta(1000),
    ts_test_utils:set_max_concurrent(1000),
%%    ts_test_utils:set_max_query_size(1000000000),

    ColList = [1, 10, 100, 200],

    %% 100 seconds will cause every 100 1-s records to be written to a
    %% different partition.  
    %%
    %%W e will test queries returning 1,10,100,1000, and 10000 records, thus 1, 1, 1, 10 and 100 quanta

    ts_test_utils:setup_ts_gen_cluster(multiple, 1, ColList, 100, "s"),

    pass.
