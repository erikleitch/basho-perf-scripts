-module(riak_prof_tests).

-compile([export_all]).

-include_lib("$RIAK_TEST_BASE/erlang_scripts/profiler/include/profiler.hrl").

%%=======================================================================
%% Utilities
%%=======================================================================

getClient(_) ->
    getClient().

getClient() ->
    {ok, C} = riakc_pb_socket:start_link("127.0.0.1", 10017),
    C.

%%=======================================================================
%% TS PUT Latency tests
%%=======================================================================

%%------------------------------------------------------------
%% Run Ncols x Nbyte trials, doing Nrow x Niter iterations for each
%% combination
%%------------------------------------------------------------

checkArgs(Args) ->
    io:format("Found Args = ~p~n", [Args]).

runTsPutLatencyTests([Nrow]) when is_list(Nrow) ->
    runTsPutLatencyTests(list_to_integer(Nrow));
runTsPutLatencyTests(Nrow) ->
    Ncols = [1, 5, 10, 20, 50],

    Bytes = [{1000,      1}, 
	     {1000,     10}, 
	     {1000,    100}, 
	     {1000,   1000}, 
	     {100,   10000}, 
	     {100,  100000}],

    C = getClient(),
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),

    ColRunFun = 
	fun(Ncol) ->
		[tsLatencyPutTest(C, Nrow, Ncol, Niter, Nbyte) || {Niter, Nbyte} <- Bytes]
	end,

    [ColRunFun(Ncol) || Ncol <- Ncols].

%%
%% Ie, for 100 bytes total, we will write 100 bytes/col for Ncol =  1
%%                                         20 bytes/col for Ncol =  5
%%                                         10 bytes/col for Ncol = 10
%%                                          5 bytes/col for Ncol = 20
%%------------------------------------------------------------

runTsSplits([Nrow]) when is_list(Nrow) ->
    runTsSplits(list_to_integer(Nrow));
runTsSplits(Nrow) ->
    Ncols = [1, 5, 10, 20, 50],

    Bytes = [{10000,   100}, 
	     {1000,   1000}, 
	     {1000,  10000}, 
	     {1000, 100000},
	     {100, 1000000}],

    C = getClient(),
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),

    ColRunFun = 
	fun(Ncol) ->
		[tsLatencyPutTest(C, Nrow, Ncol, Niter, round(NbyteTotal/Ncol)) || {Niter, NbyteTotal} <- Bytes]
	end,

    [ColRunFun(Ncol) || Ncol <- Ncols].

tsLatencyTestData(Ncol, Nbyte) ->    
    tsLatencyTestData(Ncol, Nbyte, 0, []).

tsLatencyTestData(_Ncol, _Nbyte, _Ncol, Acc) ->
    Acc;
tsLatencyTestData(Ncol, Nbyte, Icol, Acc) ->
    tsLatencyTestData(Ncol, Nbyte, Icol+1, [crypto:rand_bytes(Nbyte)|Acc]).

tsLatencyPutTest(C, Nrow, Ncol, Niter, Nbyte) ->
    Name = list_to_atom("put_" ++ integer_to_list(Ncol) ++ "_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(Nrow*Niter)),
    Bucket = "Gen" ++ integer_to_list(Ncol),
    io:format("Generating data for Bucket = ~p, Nrow = ~p Ncol = ~p Nbyte = ~p~n", [Bucket, Nrow, Ncol, Nbyte]),
    FieldData = tsLatencyTestData(Ncol, Nbyte),
    profiler:profile({start, Name}),
    tsLatencyPutTest({C, Bucket, FieldData}, Name, Nrow, 0, Niter, 1).
tsLatencyPutTest(_ArgTuple, Name, _Nrow, _Nrow, _Niter, _Niter) ->
    profiler:profile({stop, Name}),
    profiler:profile({debug}),
    ok;
tsLatencyPutTest(ArgTuple, Name, Nrow, Nrow, Niter, AccIter) ->
    tsLatencyPutTest(ArgTuple, Name, Nrow, 0, Niter, AccIter+1);
tsLatencyPutTest(ArgTuple, Name, Nrow, AccRow, Niter, AccIter) ->
    {C, Bucket, FieldData} = ArgTuple,
    Data = [list_to_tuple([<<"family1">>, <<"seriesX">>, AccRow+1, AccRow+1] ++ FieldData)],
    riakc_ts:put(C, Bucket, Data),
    tsLatencyPutTest(ArgTuple, Name, Nrow, AccRow+1, Niter, AccIter).

%%=======================================================================
%% TS INSERT Latency tests
%%=======================================================================

%%------------------------------------------------------------
%% Run Ncols x Nbyte trials, doing Nrow x Niter iterations for each
%% combination
%%------------------------------------------------------------

runTsInsertLatencyTests([Nrow, Date]) when is_list(Nrow) ->
    io:format("Running runTsInsertLantecyTests with Nrow = ~p Date = ~p~n", [Nrow, Date]),
    runTsInsertLatencyTests(list_to_integer(Nrow), Date).

runTsInsertLatencyTests(Nrow, Date) ->
    Ncols = [1, 5, 10, 20, 50],

    Bytes = [{1000,      1}, 
	     {1000,     10}, 
	     {1000,    100}, 
	     {1000,   1000}, 
	     {100,   10000}],

    C = getClient(),
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),

    ColRunFun = 
	fun(Ncol) ->
		[tsLatencyInsertTest({C, Nrow, Date}, Ncol, Niter, Nbyte) || {Niter, Nbyte} <- Bytes]
	end,

    [ColRunFun(Ncol) || Ncol <- Ncols].

tsInsertLatencyTestData(Ncol, Nbyte) ->    
    tsInsertLatencyTestData(Ncol, Nbyte, 0, "").

tsInsertLatencyTestData(_Ncol, _Nbyte, _Ncol, Acc) ->
    Acc;
tsInsertLatencyTestData(Ncol, Nbyte, Icol, Acc) ->
    case Ncol - Icol > 1 of 
	true ->
	    tsInsertLatencyTestData(Ncol, Nbyte, Icol+1, Acc ++ "'" ++ get_random_string(Nbyte) ++ "',");
	false ->
	    tsInsertLatencyTestData(Ncol, Nbyte, Icol+1, Acc ++ "'" ++ get_random_string(Nbyte) ++ "')")
    end.

tsLatencyInsertTest({C, Nrow, Date}, Ncol, Niter, Nbyte) ->
    Name = list_to_atom("put_" ++ integer_to_list(Ncol) ++ "_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(Nrow*Niter)),
    Bucket = "Gen" ++ integer_to_list(Ncol),
    QueryPrefix = "INSERT INTO " ++ Bucket ++ " (myfamily, myseries, time, myint, " ++ genFieldName(Ncol) ++ ") VALUES ('family1', 'seriesX',",
    io:format("Generating data for Bucket = ~p, Nrow = ~p Ncol = ~p Nbyte = ~p~n", [Bucket, Nrow, Ncol, Nbyte]),
    QuerySuffix = tsInsertLatencyTestData(Ncol, Nbyte),
    profiler:profile({start, Name}),
    tsLatencyInsertTest({C, QueryPrefix, QuerySuffix, Date}, Name, Nrow, 0, Niter, 1).
tsLatencyInsertTest(_ArgTuple, Name, _Nrow, _Nrow, _Niter, _Niter) ->
    profiler:profile({stop, Name}),
    profiler:profile({debug}),
    ok;
tsLatencyInsertTest(ArgTuple, Name, Nrow, Nrow, Niter, AccIter) ->
    tsLatencyInsertTest(ArgTuple, Name, Nrow, 0, Niter, AccIter+1);
tsLatencyInsertTest(ArgTuple, Name, Nrow, AccRow, Niter, AccIter) ->
    {C, QueryPrefix, QuerySuffix, Date} = ArgTuple,

    %%------------------------------------------------------------
    %% 1471219200 seconds is Mon Aug 15 00:00:00 UTC 2016
    %%------------------------------------------------------------

    IntVal = integer_to_list(1471219200000 + AccRow),
    Hours = AccRow div (1000*3600),
    Min = (AccRow - Hours*1000*3600) div (1000*60),
    Sec = (AccRow - Hours*1000*3600 - Min*1000*60) div 1000,
    Ms  = (AccRow - Hours*1000*3600 - Min*1000*60 - Sec*1000),
    DateTime = binary_to_list(iolist_to_binary(io_lib:format("2016-08-15T~2..0B:~2..0B:~2..0B.~3..0BZ", [Hours, Min, Sec, Ms]))),

    Query = 
	case Date of 
	    true ->
		QueryPrefix ++ "'" ++ DateTime ++ "', " ++ IntVal ++ ", " ++ QuerySuffix;
	    _ ->
		QueryPrefix ++ IntVal ++ ", " ++ IntVal ++ ", " ++ QuerySuffix
	end,

    {ok, _} = riakc_ts:query(C, Query),
    tsLatencyInsertTest(ArgTuple, Name, Nrow, AccRow+1, Niter, AccIter).

runTest(Date) ->
    C = getClient(),
    tsLatencyInsertTest({C, 10, Date}, 5, 1, 10).

genFieldName(Ind) ->
    genFieldNames(Ind, 0, []).
genFieldNames(_Ind, _Ind, Acc) ->
    Acc;
genFieldNames(Ind, Counter, Acc) ->
    case Ind - Counter > 1 of
	true ->
	    genFieldNames(Ind, Counter+1, Acc ++ "myvar" ++ integer_to_list(Counter+1) ++ ", ");
	_ ->
	    genFieldNames(Ind, Counter+1, Acc ++ "myvar" ++ integer_to_list(Counter+1))
    end.

get_random_string(Length) ->
    AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).

%%========================================================================
%% TS query latency tests
%%========================================================================

runTsQueryLatencyTests([Nbyte, Select, Group]) when is_list(Nbyte) ->
    io:format("Nbyte = ~p Slect = ~p Group = ~p~n", [Nbyte, Select, Group]),
    runTsQueryLatencyTests(list_to_integer(Nbyte), list_to_atom(Select), list_to_atom(Group)).

runTsQueryLatencyTests(Nbyte, Select, Group) ->
    Ncols = [1, 5, 10, 20, 50],

    Rows = [{100,       1}, 
	    {100,      10}, 
	    {100,     100}, 
	    {100,    1000}, 
	    {10,    10000}], 

    C = getClient(),
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),

    ColRunFun = 
	fun(Ncol) ->
		[tsLatencyQueryTest({C, Select, Group}, Nrow, Ncol, Niter, Nbyte) || {Niter, Nrow} <- Rows]
	end,

    [ColRunFun(Ncol) || Ncol <- Ncols].

tsLatencyQueryTest({C, Select, Group}, Nrow, Ncol, Niter, Nbyte) ->

    FieldData = tsLatencyTestData(Ncol, Nbyte),
    Bucket = "Gen" ++ integer_to_list(Ncol),

    %%------------------------------------------------------------
    %% Put generated data
    %%------------------------------------------------------------

    io:format("Putting data for Ncol = ~p Nrow = ~p ~n", [Ncol, Nrow]),

    putQueryTestData({C, Bucket, FieldData}, Nrow, 0),
    
    Name  = list_to_atom("query_" ++ integer_to_list(Ncol) ++ "_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(Nrow) ++ "_" ++ integer_to_list(Niter)),

    FieldCond = 
	case Select of
	    all ->
		"*";
	    _ ->
		atom_to_list(Select)
	end,

    GroupCond = 
	case Group of
	    none ->
		"";
	    _ ->
		" group by " ++ atom_to_list(Group)
	end,

    Query = "select " ++ FieldCond ++ " from Gen" ++ integer_to_list(Ncol) ++ " where myfamily='family1' and myseries='seriesX' and time > 0 and time <= " ++ integer_to_list(Nrow) ++ GroupCond,

    %%------------------------------------------------------------
    %% Now start the profiler for Niter iterations of this combination
    %% rows, columns and bytes per column
    %%------------------------------------------------------------

    io:format("Querying data for Ncol = ~p Nrow = ~p with Query = ~p~n", [Ncol, Nrow, Query]),

    profiler:profile({start, Name}),
    tsLatencyQueryTest({C, Query, Name}, Niter, 0).

tsLatencyQueryTest({_C, _Query, Name}, _Niter, _Niter) ->
    profiler:profile({stop, Name}),
    profiler:profile({debug}),
    ok;
tsLatencyQueryTest(Args, Niter, AccIter) ->
    {C, Query, _Name} = Args,
    {ok, _} = riakc_ts:query(C, Query),
    tsLatencyQueryTest(Args, Niter, AccIter+1).

putQueryTestData(Args, Nrow, AccRow) ->
    putQueryTestData(Args, Nrow, AccRow, 0).

putQueryTestData(_Args, _Nrow, _Nrow, _Ind) ->
    ok;
putQueryTestData(Args, Nrow, AccRow, GenInd) ->
    {C, Bucket, FieldData} = Args,

    %%------------------------------------------------------------
    %% We will generate data with the same integer index for 10% of
    %% the total data length
    %%------------------------------------------------------------

    RowsPerInd = 
	case Nrow > 10 of
	    true ->
		Nrow div 10;
	    _ ->
		1
	end,

    NewInd = 
	case AccRow rem RowsPerInd of 
	    0 ->
		random:uniform(100);
	    _ ->
		GenInd
	end,

    Data = [list_to_tuple([<<"family1">>, <<"seriesX">>, AccRow+1, NewInd] ++ FieldData)],
    riakc_ts:put(C, Bucket, Data),
    putQueryTestData(Args, Nrow, AccRow+1, NewInd).

%%-----------------------------------------------------------------------
%% TS cluster population tests
%%-----------------------------------------------------------------------

putRandomTsData([Nrow, MsRange]) when is_list(Nrow) ->
    io:format("Running putRandomTsData with Nrow = ~p MsRange = ~p~n", [Nrow, MsRange]),
    putRandomTsData(list_to_integer(Nrow), list_to_integer(MsRange)).

putRandomTsData(Nrow, MsRange) ->
    C = getClient(),
    Bucket = <<"GeoCheckin">>,
    putRandomTsData({C, Bucket, MsRange}, Nrow, 0).

putRandomTsData(_Args, _Nrow, _Nrow) ->
    ok;
putRandomTsData(Args, Nrow, AccRow) ->
    {C, Bucket, MsRange} = Args,
    Data = [list_to_tuple([<<"family1">>, <<"seriesX">>, random:uniform(MsRange)+1, 1, <<"binval">>, 1.234, true])],
    riakc_ts:put(C, Bucket, Data),
    putRandomTsData(Args, Nrow, AccRow+1).

%%----------------------------------------------------------------------- 
%% KV PUT/GET Latency tests
%%----------------------------------------------------------------------- 

runKvLatencyTests() ->
    Bytes = [{10000,       1}, 
	     {10000,      10}, 
	     {10000,     100}, 
	     {10000,    1000}, 
	     {10000,   10000}, 
	     {1000,   100000}, 
	     {1000,  1000000}, 
	     {100,  10000000}],
    [runKvLatencyTest(N, Nbyte) || {N, Nbyte} <- Bytes].

runKvLatencyTest(N, Nbyte) ->
    kvPutTest(N, Nbyte),
    kvGetTest(N, Nbyte),
    kvDelTest(N, Nbyte).
    
runKvLatencyTest(Args) ->
    [Nstr, NbyteStr] = Args,
    N = list_to_integer(Nstr),
    Nbyte = list_to_integer(NbyteStr),
    kvPutTest(N, Nbyte),
    kvGetTest(N, Nbyte).
     
kvTestData(Nbyte) ->
    crypto:rand_bytes(Nbyte).

kvPutTest(N,Nbyte) ->
    C = getClient(),
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),
    Name = list_to_atom("put_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(N)),
    profiler:profile({start, Name}),
    io:format("Generating data ~n", []),
    Data = kvTestData(Nbyte),
    io:format("Done generating data ~n", []),
    kvPutTest(C,Data,Name,N,0).
kvPutTest(_C,_Data,Name,_N,_N) ->
    profiler:profile({stop, Name}),
    profiler:profile({debug}),
    ok;
kvPutTest(C,Data,Name, N,Acc) ->
    Obj = riakc_obj:new({<<"TestBucketType">>, <<"GeoCheckin">>}, <<"key1">>, Data),
    riakc_pb_socket:put(C, Obj),
    kvPutTest(C,Data,Name,N,Acc+1).

kvGetTest(N,Nbyte) ->
    C = getClient(),
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),
    Name = list_to_atom("get_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(N)),
    profiler:profile({start, Name}),
    kvGetTest(C,Name,N,0).
kvGetTest(_C,Name,_N,_N) ->
    profiler:profile({stop, Name}),
    profiler:profile({debug}),
    ok;
kvGetTest(C,Name,N,Acc) ->
    riakc_pb_socket:get(C, {<<"TestBucketType">>, <<"GeoCheckin">>}, <<"key1">>),
    kvGetTest(C,Name,N,Acc+1).

kvDelTest(N,Nbyte) ->
    C = getClient(),
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),
    Name = list_to_atom("del_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(N)),
    profiler:profile({start, Name}),
    kvDelTest(C,Name,N,0).
kvDelTest(_C,Name,_N,_N) ->
    profiler:profile({stop, Name}),
    profiler:profile({debug}),
    ok;
kvDelTest(C,Name,N,Acc) ->
    riakc_pb_socket:delete(C, {<<"TestBucketType">>, <<"GeoCheckin">>}, <<"key1">>),
    kvDelTest(C,Name,N,Acc+1).

keys(Bucket) ->
    {ok, Riak} = riakc_pb_socket:start_link("127.0.0.1", 10017),
    riakc_ts:stream_list_keys(Riak, Bucket, []),
    receive_keys([]).

receive_keys(Keys) ->
    receive
	{_, {keys, KeyList}} ->
	    receive_keys(Keys ++ KeyList);
	{_, done} ->
	    Keys;
	Ret ->
	    io:format(user, "Ret = ~p~n", [Ret])
    end.
