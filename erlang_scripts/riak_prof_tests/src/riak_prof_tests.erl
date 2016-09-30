-module(riak_prof_tests).

-compile([export_all]).

-include_lib("$RIAK_TEST_BASE/erlang_scripts/profiler/include/profiler.hrl").

%%=======================================================================
%% Utilities
%%=======================================================================

getClient(Ip, Port) ->
    {ok, C} = riakc_pb_socket:start_link(Ip, Port),
    C.
    
getClient() ->
    getClient("127.0.0.1", 10017).

getClient(slb) ->
    getClient("10.109.234.238", 8087);
getClient(_) ->
    getClient("127.0.0.1", 10017).

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

putSequentialTsData([Nrow, MsDelta]) when is_list(Nrow) ->
    putSequentialTsData(list_to_integer(Nrow, MsDelta)).

putSequentialTsData(Nrow, MsDelta) ->
    C = getClient(),
    Bucket = <<"GeoCheckin">>,
    putSequentialTsData({C, Bucket, MsDelta}, Nrow, 0).

putSequentialTsData(_Args, _Nrow, _Nrow) ->
    ok;
putSequentialTsData(Args, Nrow, AccRow) ->
    {C, Bucket, MsDelta} = Args,
    Data = [list_to_tuple([<<"family1">>, <<"seriesX">>, (AccRow+1)*MsDelta, 1, <<"binval">>, 1.234, true])],
    riakc_ts:put(C, Bucket, Data),
    putSequentialTsData(Args, Nrow, AccRow+1).

%%=======================================================================
%% Intellicore testing
%%=======================================================================

%%-----------------------------------------------------------------------
%% Populate
%%-----------------------------------------------------------------------
putSequentialIntellicoreData([Nrow, StartTime, StopTime]) when is_list(Nrow) ->
    putSequentialTsData(list_to_integer(Nrow), {StartTime, StopTime}).

putSequentialIntellicoreData(Nrow, {StartTime, StopTime, LapsFrac}) ->
    C = getClient(),
    Bucket = <<"TIM_motorsport_formula_e_2015">>,
    Delta = round((StopTime - StartTime) / Nrow),
    putSequentialIntellicoreData({C, Bucket, StartTime, Delta, LapsFrac}, Nrow, 0).

putSequentialIntellicoreData(_Args, _Nrow, _Nrow) ->
    ok;
putSequentialIntellicoreData(Args, Nrow, AccRow) ->
    {C, Bucket, StartTime, Delta, LapsFrac} = Args,
    Data = getIntellicoreData(<<"596044d8-86f5-462f-8d94-65b25e7d3fe9">>, StartTime + AccRow * Delta, LapsFrac),

    case AccRow rem 1000 of 
	0 ->
	    io:format("Putting row ~p for Bucket ~p Data = ~p~n", [AccRow, Bucket, Data]);
	_ ->
	    ok
    end,

    riakc_ts:put(C, Bucket, Data),
    putSequentialIntellicoreData(Args, Nrow, AccRow+1).

intellicoreTest() ->
    putSequentialIntellicoreData(137000, {1467554400000, 1467563400000, 1404.0/137000}).

getIntellicoreData(SportEventUuid, Timestamp, LapsFrac) ->
    VarcharSize = length(binary_to_list(SportEventUuid)),

    LapsRand = random:uniform(1000),
    LapsComp = LapsFrac * 1000,
    Laps = 
	case LapsRand =< LapsComp of
	    true ->
		10;
	    false ->
		-1
	end,
    [list_to_tuple([SportEventUuid, 
		    Timestamp, 
		    list_to_binary(get_random_string(VarcharSize)),
		    list_to_binary(get_random_string(VarcharSize)),
		    list_to_binary(get_random_string(VarcharSize)),
		    list_to_binary(get_random_string(VarcharSize)),
		    list_to_binary(get_random_string(VarcharSize)),
		    list_to_binary(get_random_string(VarcharSize)),
		    list_to_binary(get_random_string(VarcharSize)),
		    false,
		    
		    float(random:uniform(100)),
		    random:uniform(100),
		    
		    float(random:uniform(100)),
		    random:uniform(100),
		    
		    float(random:uniform(100)),
		    float(random:uniform(100)),
		    float(random:uniform(100)),
		    float(random:uniform(100)),
		    float(random:uniform(100)),
		    
		    random:uniform(100),
		    float(random:uniform(100)),
		    
		    %% This next is laps

		    Laps,
		    random:uniform(100),
		    random:uniform(100),
		    
		    float(random:uniform(100)),
		    float(random:uniform(100)),
		    float(random:uniform(100)),
		    
		    list_to_binary(get_random_string(VarcharSize))])].

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
    keys(getClient(), Bucket).

keys(C, Bucket) ->
    riakc_ts:stream_list_keys(C, Bucket, []),
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

geoQuery(Range) ->
    C = getClient(),
    profiler:profile({start, test}),
    riakc_ts:query(C, "select * from GeoCheckin where myfamily='family1' and myseries='seriesX' and time > 0 and time < " ++ integer_to_list(Range)),
    profiler:profile({stop, test}),
    profiler:profile({debug}).

query(Q, Atom) ->
	C = getClient(),
	profiler:profile({start, Atom}),
	{ok, {_Cols, Rows}} = riakc_ts:query(C, Q),
        profiler:profile({stop, Atom}),
	profiler:profile({debug}),
        io:format("Query returned ~p rows~n", [length(Rows)]),
        Rows.

iq(all, Atom) ->
    Q = "select * from TIM_motorsport_formula_e_2015 where sport_event_uuid = '596044d8-86f5-462f-8d94-65b25e7d3fe9' and time >= 1467554400000 and time < 1467563400000",
    query(Q, Atom);
iq(part, Atom) ->
    EndTime = 1467554400000 + round((1467563400000 - 1467554400000)/137),
    Q = "select * from TIM_motorsport_formula_e_2015 where sport_event_uuid = '596044d8-86f5-462f-8d94-65b25e7d3fe9' and time >= 1467554400000 and time < " ++ integer_to_list(EndTime),
    query(Q, Atom);
iq(laps, Atom) ->
    Q = "select * from TIM_motorsport_formula_e_2015 where sport_event_uuid = '596044d8-86f5-462f-8d94-65b25e7d3fe9' and time >= 1467554400000 and time < 1467563400000 and laps > 0",
    query(Q, Atom);
iq(full, Atom) ->
    Q = "select max(laps), driver_number from TIM_motorsport_formula_e_2015 where sport_event_uuid = '596044d8-86f5-462f-8d94-65b25e7d3fe9' and time >= 1467554400000 and time < 1467563400000 and laps >= 0 group by driver_number",
    query(Q, Atom).

getDb(0) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/0",
    getDb(File);
getDb(1096) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/1096126227998177188652763624537212264741949407232",
    getDb(File);
getDb(1278) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/1278813932664540053428224228626747642198940975104",
    getDb(File);
getDb(1826) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/182687704666362864775460604089535377456991567872",
    getDb(File);
getDb(3653) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/365375409332725729550921208179070754913983135744",
    getDb(File);
getDb(5480) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/548063113999088594326381812268606132370974703616",
    getDb(File);
getDb(7307) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/730750818665451459101842416358141509827966271488",
    getDb(File);
getDb(9134) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/913438523331814323877303020447676887284957839360",
    getDb(File);
getDb(File) ->
    LockFile = File ++ "/LOCK",
    os:cmd("rm " ++ LockFile),
    Opts = [{total_memory, 14307360768}, {block_cache_threshold, 33554432}, {block_restart_interval, 16}, {block_size_steps, 16}, {cache_object_warming, true}, {compression, lz4}, {create_if_missing, true}, {delete_threshold, 1000}, {eleveldb_threads, 71}, {expiry_enabled, false}, {expiry_minutes, 0}, {fadvise_willneed, false}, {limited_developer_mem, true}, {sst_block_size, 4096}, {tiered_slow_level, 0}, {total_leveldb_mem_percent, 70}, {use_bloomfilter, true}, {whole_file_expiry, true}, {write_buffer_size, 39817495}],
    {ok, DbRef} = eleveldb:open(File, Opts),
    DbRef.

iteratorTest(0) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/0",
    iteratorTest(File);
iteratorTest(1096) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/1096126227998177188652763624537212264741949407232",
    iteratorTest(File);
iteratorTest(1278) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/1278813932664540053428224228626747642198940975104",
    iteratorTest(File);
iteratorTest(1826) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/182687704666362864775460604089535377456991567872",
    iteratorTest(File);
iteratorTest(3653) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/365375409332725729550921208179070754913983135744",
    iteratorTest(File);
iteratorTest(5480) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/548063113999088594326381812268606132370974703616",
    iteratorTest(File);
iteratorTest(7307) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/730750818665451459101842416358141509827966271488",
    iteratorTest(File);
iteratorTest(9134) ->
    File="/Users/eml/rt/riak/current/dev/dev1/data/leveldb/913438523331814323877303020447676887284957839360",
    iteratorTest(File);

iteratorTest(File) ->
    LockFile = File ++ "/LOCK",
    os:cmd("rm " ++ LockFile),
    Opts = [{total_memory, 14307360768}, {block_cache_threshold, 33554432}, {block_restart_interval, 16}, {block_size_steps, 16}, {cache_object_warming, true}, {compression, lz4}, {create_if_missing, true}, {delete_threshold, 1000}, {eleveldb_threads, 71}, {expiry_enabled, false}, {expiry_minutes, 0}, {fadvise_willneed, false}, {limited_developer_mem, true}, {sst_block_size, 4096}, {tiered_slow_level, 0}, {total_leveldb_mem_percent, 70}, {use_bloomfilter, true}, {whole_file_expiry, true}, {write_buffer_size, 39817495}],

    {ok, DbRef} = eleveldb:open(File, Opts),
    {ok, Iter} = eleveldb:iterator(DbRef, []),

    try 
	{ok, Key, Val} = eleveldb:iterator_move(Iter, first),
	io:format("Key = ~p Val = ~p~n", [Key, Val]),
	printNextKey(DbRef, Iter)
    catch
	Err:Msg ->
	    io:format("Caught an error: ~p ~p~n", [Err, Msg]),
	    ok
    after
	eleveldb:iterator_close(Iter),
	eleveldb:close(DbRef)
    end.

printNextKey(DbRef, Iter) ->
    {ok, Key, Val} = eleveldb:iterator_move(Iter, next),
    DecodedKey = riak_kv_eleveldb_backend:orig_from_object_key(Key),
    DecodedValue = riak_object:get_values(riak_object:from_binary(<<"GeoCheckin">>, Key, Val)),
    io:format("Key = ~p Value = ~p~n", [DecodedKey, DecodedValue]),
    printNextKey(DbRef, Iter).

countKeys(File) ->
    LockFile = File ++ "/LOCK",
    os:cmd("rm " ++ LockFile),
    Opts = [{total_memory, 14307360768}, {block_cache_threshold, 33554432}, {block_restart_interval, 16}, {block_size_steps, 16}, {cache_object_warming, true}, {compression, lz4}, {create_if_missing, true}, {delete_threshold, 1000}, {eleveldb_threads, 71}, {expiry_enabled, false}, {expiry_minutes, 0}, {fadvise_willneed, false}, {limited_developer_mem, true}, {sst_block_size, 4096}, {tiered_slow_level, 0}, {total_leveldb_mem_percent, 70}, {use_bloomfilter, true}, {whole_file_expiry, true}, {write_buffer_size, 39817495}],

    {ok, DbRef} = eleveldb:open(File, Opts),
    {ok, Iter} = eleveldb:iterator(DbRef, []),

    Nkeys = 
    try 
	{ok, _Key, _Val} = eleveldb:iterator_move(Iter, first),
	countKey(DbRef, Iter, 0)
    catch
	Err:Msg ->
	    io:format("Caught an error: ~p ~p~n", [Err, Msg]),
	    ok
    after
	eleveldb:iterator_close(Iter),
	eleveldb:close(DbRef)
    end,
    Nkeys.

countKey(DbRef, Iter, Acc) ->
    case eleveldb:iterator_move(Iter, next) of
	{ok, _Key, _Val} ->
	    countKey(DbRef, Iter, Acc+1);
	{error, invalid_iterator} ->
	    Acc
    end.

dbFiles() ->
    Nodes = [0,182687704666362864775460604089535377456991567872,
             365375409332725729550921208179070754913983135744,
             548063113999088594326381812268606132370974703616,
             730750818665451459101842416358141509827966271488,
             913438523331814323877303020447676887284957839360,
             1096126227998177188652763624537212264741949407232,
	     1278813932664540053428224228626747642198940975104],
    ["/Users/eml/rt/riak/current/dev/dev1/data/leveldb/" ++ integer_to_list(Node) || Node <- Nodes].

printLeveldbBytes() ->
    printLeveldbBytes(dbFiles()).

printLeveldbBytes([File]) when is_list(File) ->
    io:format("~p~n", [getLeveldbBytes(File)]);
printLeveldbBytes(List) ->
    [io:format("~p~n", [getLeveldbBytes(File)]) || File <- List].

getLeveldbBytes(DbFile) ->
    LockFile = DbFile ++ "/LOCK",
    os:cmd("rm " ++ LockFile),
    {ok, DbRef} = eleveldb:open(DbFile, [{create_if_missing, true}]),
    {ok, Bytes} = eleveldb:status(DbRef, <<"leveldb.total-bytes">>),
    binary_to_integer(Bytes).

printLeveldbKeys([File]) when is_list(File) ->
    io:format("~p~n", [iteratorTest(File)]);
printLeveldbKeys(List) ->
    [io:format("~p~n", [iteratorTest(File)]) || File <- List].

countLeveldbKeys([File]) when is_list(File) ->
    io:format("~p~n", [countKeys(File)]);
countLeveldbKeys(List) ->
    [io:format("~p~n", [countKeys(File)]) || File <- List].

testFn([File]) when is_list(File) ->
    io:format("~p~n", [File]).
