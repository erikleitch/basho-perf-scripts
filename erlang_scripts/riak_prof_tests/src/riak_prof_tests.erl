-module(riak_prof_tests).

-compile([export_all]).

%%-include_lib("$RIAK_TEST_BASE/erlang_scripts/profiler/include/profiler.hrl").

%%=======================================================================
%% Utilities
%%=======================================================================

getClient(Ip, Port) ->
    {ok, C} = riakc_pb_socket:start_link(Ip, Port),
    C.
    
getClient() ->
    getClient("127.0.0.1", 10017).

getClient(sl) ->
    getClient(slb);
getClient('softlayer-b') ->
    getClient(slb);
getClient('softlayer-dev-a') ->
    getClient(sldeva);
getClient(sldeva) ->
    getClient("10.109.234.226", 8087);
getClient(slb) ->
    getClient("10.109.234.238", 8087);
getClient(aws) ->
    getClient("10.1.3.1", 8087);
getClient(_) ->
    getClient("127.0.0.1", 10017).

%%=======================================================================
%% TS PUT Latency tests
%%=======================================================================

%%------------------------------------------------------------
%% Run Ncols x Nbyte trials, doing Nrow x Niter iterations for each
%% combination
%%------------------------------------------------------------

checkArgs([ArgStr]) ->
    ArgDict = getArgDict(ArgStr),
    io:format("Found dictionary = ~p~n", [ArgDict]),
    io:format("Found key arg1 = ~p~n", [getArgKey(ArgDict, "arg1", [1,10,20])]).

%%------------------------------------------------------------
%% Convert a continuous string of the form "key1=val1,key2=val2" to a
%% dictionary of key,val pairs
%%------------------------------------------------------------

getArgDict(ArgStr) ->
    ArgList = string:tokens(ArgStr, ","),
    KeyValParser = 
	fun(Arg) ->
		[Key, Val] = string:tokens(Arg, "="),

		io:format("Processing Arg = ~p Key = ~p Val = ~p~n", [Arg, Key, Val]),

		{KeyName, KeyType} = parseKey(Key),

		io:format("Processing Keyname = ~p Type = ~p~n", [KeyName, KeyType]),

		ConvVal = parseVal(Val, KeyType),

		io:format("Processing ConvVal = ~p ~n", [ConvVal]),

		{KeyName, ConvVal}
	end,
	    
    KeyValList = [KeyValParser(Arg) || Arg <- ArgList],
    dict:from_list(KeyValList).

%%------------------------------------------------------------
%% Convert a value potentially of the form val1+val2+val3 into a list
%% of vals, or a single val
%%------------------------------------------------------------

parseVal(Val, Type) ->
    ValToks = string:tokens(Val, "+"),
    ValContents = 
	case ValToks of
	    [Val] ->
		Val;
	    _ ->
		io:format("Returning ValToks = ~p~n", [ValToks]),
		ValToks
	end,
    convertVal(ValContents, Type).

%%------------------------------------------------------------
%% Convert a value potentially consisting of a list, to the indicated
%% type
%%------------------------------------------------------------

convertVal(Val, Type) ->
    io:format("Inside convertVal with Val = ~p Type = ~p~n", [Val, Type]),
    case io_lib:printable_list(Val) of
	true ->
	    convertSingleVal(Val, Type);
	_ ->
	    convertListVal(Val, Type)
    end.

convertSingleVal(Val, Type) ->
    io:format("Inside CV2 with Val = ~p~n", [Val]),
    case Type of
	none ->
	    Val;
	atom ->
	    list_to_atom(Val);
	int  ->
	    io:format("Calling ltoi on Val = ~p~n", [Val]),
	    list_to_integer(Val)
    end.

convertListVal(ValList, Type) ->
    io:format("Inside CV1 with ValList = ~p~n", [ValList]),
    [convertSingleVal(Val, Type) || Val <- ValList].

%%------------------------------------------------------------
%% Parse a key, potentially of the form "key_type" into a {Key, Type}
%% tuple
%%------------------------------------------------------------

parseKey(Key) ->
    KeyToks = string:tokens(Key, "_"),
    case KeyToks of 
	[Key] ->
	    {Key, none};
	[Name, Type] ->
	    {Name, list_to_atom(Type)}
	end.

%%------------------------------------------------------------
%% Given a dictionary, return the key value, or the default value if
%% key doesn't exist
%%------------------------------------------------------------

getArgKey(Dict, Key, DefVal) ->
    case dict:is_key(Key, Dict) of
	true ->
	    dict:fetch(Key, Dict);
	_ ->
	    DefVal
    end.
	    
runTsPutLatencyTests([Nrow, StrNcols]) when is_list(Nrow) ->
    SNcolsList = string:tokens(StrNcols, "+"),
    NcolsList = [list_to_integer(Item) || Item <- SNcolsList],
    runTsPutLatencyTests(list_to_integer(Nrow), NcolsList).

runTsPutLatencyTests(Nrow, Ncols) ->

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

percTest(Nrow) ->
        runTsPutLatencyPercentileTests([integer_to_list(Nrow), "1+10+20+50"]).

runTsPutLatencyPercentileTests([Nrow, StrNcols]) when is_list(Nrow) ->
    SNcolsList = string:tokens(StrNcols, "+"),
    NcolsList = [list_to_integer(Item) || Item <- SNcolsList],
    runTsPutLatencyPercentileTests(list_to_integer(Nrow), NcolsList).

runTsPutLatencyPercentileTests(Nrow, Ncols) ->

    Bytes = [{1,       1}, 
	     {1,      10}, 
	     {1,     100}, 
	     {1,    1000}, 
	     {1,   10000}],

    C = getClient(),
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),

    ColRunFun = 
	fun(Ncol) ->
		[tsLatencyPercentilePutTest(C, Nrow, Ncol, Niter, Nbyte) || {Niter, Nbyte} <- Bytes]
	end,

    [ColRunFun(Ncol) || Ncol <- Ncols].

runTsPutInsertLatencyTests([Nrow, StrNcols]) when is_list(Nrow) ->
    SNcolsList = string:tokens(StrNcols, "+"),
    NcolsList = [list_to_integer(Item) || Item <- SNcolsList],
    runTsPutInsertLatencyTests(list_to_integer(Nrow), NcolsList).

runTsPutInsertLatencyTests(Nrow, Ncols) ->
%%    Ncols = [1, 5, 10, 20, 50],

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
		[tsLatencyPutInsertTest(C, Nrow, Ncol, Niter, Nbyte) || {Niter, Nbyte} <- Bytes]
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

%%=======================================================================
%% TS Put Latency Tests
%%=======================================================================

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
    Data = [list_to_tuple([<<"family1">>, <<"seriesX">>, AccRow+1] ++ FieldData ++ [AccRow+1])],
    ok = riakc_ts:put(C, Bucket, Data),
    tsLatencyPutTest(ArgTuple, Name, Nrow, AccRow+1, Niter, AccIter).

%%------------------------------------------------------------
%% PUT percentile latencies
%%------------------------------------------------------------

append_to_file(Filename, Format, Data) ->
    case file:open(Filename, [append]) of
        {ok, IoDevice} ->
	    Bytes = io:format(Format, [Data]),
            file:write(IoDevice, Bytes),
	    file:close(IoDevice);
	        {error, Reason} ->
            io:format("~s open error  reason:~s~n", [Filename, Reason])
    end.

tsLatencyPercentilePutTest(C, Nrow, Ncol, Niter, Nbyte) ->
    Name = list_to_atom("put_ncol" ++ integer_to_list(Ncol) ++ "_nbyte" ++ integer_to_list(Nbyte) ++ "_nrow" ++ integer_to_list(Nrow*Niter)),
    Bucket = "Gen" ++ integer_to_list(Ncol),
    FileName = "/tmp/tslatency_perc/" ++ atom_to_list(Name),
    case file:open(FileName, [append]) of
        {ok, IoDevice} ->
	    IoDevice,
	    io:format("Generating data for Bucket = ~p, Nrow = ~p Ncol = ~p Nbyte = ~p~n", [Bucket, Nrow, Ncol, Nbyte]),
	    FieldData = tsLatencyTestData(Ncol, Nbyte),
	    tsLatencyPercentilePutTestLoop({C, Bucket, FieldData, IoDevice}, Nrow, 0, Niter, 1);
	{error, Reason} ->
            io:format("~s open error  reason:~s~n", [FileName, Reason])
    end.

tsLatencyPercentilePutTestLoop(ArgTuple, _Nrow, _Nrow, _Niter, _Niter) ->
    {_C, _Bucket, _FieldData, IoDevice} = ArgTuple,
    file:close(IoDevice),
    ok;
tsLatencyPercentilePutTestLoop(ArgTuple, Nrow, Nrow, Niter, AccIter) ->
    tsLatencyPercentilePutTestLoop(ArgTuple, Nrow, 0, Niter, AccIter+1);
tsLatencyPercentilePutTestLoop(ArgTuple, Nrow, AccRow, Niter, AccIter) ->
    {C, Bucket, FieldData, IoDevice} = ArgTuple,
    Data = [list_to_tuple([<<"family1">>, <<"seriesX">>, AccRow+1] ++ FieldData ++ [AccRow+1])],

    StartTime = eleveldb:current_usec(),
    ok = riakc_ts:put(C, Bucket, Data),
    StopTime = eleveldb:current_usec(),

    Bytes = io_lib:format("~p~n", [StopTime - StartTime]),
    file:write(IoDevice, Bytes),

    tsLatencyPercentilePutTestLoop(ArgTuple, Nrow, AccRow+1, Niter, AccIter).

%%------------------------------------------------------------
%% Puts as INSERTs
%%------------------------------------------------------------

tsLatencyPutInsertTest(C, Nrow, Ncol, Niter, Nbyte) ->
    Name = list_to_atom("put_" ++ integer_to_list(Ncol) ++ "_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(Nrow*Niter)),
    Bucket = "Gen" ++ integer_to_list(Ncol),
    QueryPrefix = "INSERT INTO " ++ Bucket ++ " (myfamily, myseries, time, " ++ genFieldName(Ncol) ++ ", myint) VALUES ('family1', 'seriesX',",
    io:format("Generating data for Bucket = ~p, Nrow = ~p Ncol = ~p Nbyte = ~p~n", [Bucket, Nrow, Ncol, Nbyte]),
    QuerySuffix = tsInsertLatencyTestData(Ncol, Nbyte),

    profiler:profile({start, Name}),
    tsLatencyPutInsertTest({C, QueryPrefix, QuerySuffix}, Name, Nrow, 0, Niter, 1).
tsLatencyPutInsertTest(_ArgTuple, Name, _Nrow, _Nrow, _Niter, _Niter) ->
    profiler:profile({stop, Name}),
    profiler:profile({debug}),
    ok;
tsLatencyPutInsertTest(ArgTuple, Name, Nrow, Nrow, Niter, AccIter) ->
    tsLatencyPutInsertTest(ArgTuple, Name, Nrow, 0, Niter, AccIter+1);
tsLatencyPutInsertTest(ArgTuple, Name, Nrow, AccRow, Niter, AccIter) ->
    {C, QueryPrefix, QuerySuffix} = ArgTuple,
    IntVal = integer_to_list(AccRow+1),
    Query = QueryPrefix ++ IntVal ++ ", " ++ QuerySuffix ++ ", " ++ IntVal ++ ")",
    {ok, _} = riakc_ts:query(C, Query),
    tsLatencyPutInsertTest(ArgTuple, Name, Nrow, AccRow+1, Niter, AccIter).

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
	    tsInsertLatencyTestData(Ncol, Nbyte, Icol+1, Acc ++ "'" ++ get_random_string(Nbyte) ++ "'")
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

%%------------------------------------------------------------
%% runTsQueryLatencyTests([Nbyte, Select, Group, Limit, Filter, PutData, IntervalMs, Ncols])
%%
%% Nbyte      -- number of bytes per column
%% Select     -- 'all' to select *, otherwise literal condition
%% Group      -- 'none' not to group, otherwise "group by " ++ atom_to_list(Group)
%% Limit      -- 'nolimit' to use no limit, 'limit' to limit on Group ('limits' to all Nrow results)
%% Filter     -- 'none' to use no filter, otherwise includes filter on 'myint <= Nrow'
%% PutData    -- true to put data on this iteration, false not to assume data have aready been put)
%% IntervalMs -- interval, in ms, between successive timestamps
%% Ncols      -- string of the form 1+10+20+50 to execute over [1,10,20,50] columns
%%------------------------------------------------------------

runTsQueryLatencyTests([Nbyte, Select, Group, Limit, Filter, PutData, IntervalMs, StrNcols]) when is_list(Nbyte) ->
    SNcolsList = string:tokens(StrNcols, "+"),
    NcolsList = [list_to_integer(Item) || Item <- SNcolsList],

    Rows = [{10,    10000, list_to_atom(PutData)}, 
	    {10,     5000, false}, 
	    {10,     1000, false}, 
	    {10,      100, false}, 
	    {10,       10, false}, 
	    {10,        1, false}],

    runTsQueryLatencyTests(local, list_to_integer(Nbyte), list_to_atom(Select), list_to_atom(Group), list_to_atom(Limit), list_to_atom(Filter), list_to_integer(IntervalMs), NcolsList, Rows).

runTsQueryLatencyTestsQuick([Nbyte, Select, Group, Limit, Filter, PutData, IntervalMs, StrNcols]) when is_list(Nbyte) ->
    SNcolsList = string:tokens(StrNcols, "+"),
    NcolsList = [list_to_integer(Item) || Item <- SNcolsList],

    Rows = [{1,  10000, list_to_atom(PutData)}, 
	    {1,   5000, false}, 
	    {1,   1000, false}, 
	    {1,    100, false}, 
	    {1,     10, false}, 
	    {1,      1, false}],

    runTsQueryLatencyTests(local, list_to_integer(Nbyte), list_to_atom(Select), list_to_atom(Group), list_to_atom(Limit), list_to_atom(Filter), list_to_integer(IntervalMs), NcolsList, Rows).

runTsQueryLatencyTestsSL([Nbyte, Select, Group, Limit, Filter, PutData, IntervalMs, StrNcols, Host]) when is_list(Nbyte) ->
    SNcolsList = string:tokens(StrNcols, "+"),
    NcolsList = [list_to_integer(Item) || Item <- SNcolsList],

    Rows = [{1,     10000, list_to_atom(PutData)}, 
	    {10,     5000, false}, 
	    {100,    1000, false}, 
	    {100,     100, false}, 
	    {100,      10, false}, 
	    {100,       1, false}],

    runTsQueryLatencyTests(list_to_atom(Host), list_to_integer(Nbyte), list_to_atom(Select), list_to_atom(Group), list_to_atom(Limit), list_to_atom(Filter), list_to_integer(IntervalMs), NcolsList, Rows).

runTsQueryLatencyTests(ClientAtom, Nbyte, Select, Group, Limit, Filter, IntervalMs, Ncols, Rows) ->

    io:format("Received Ncols = ~p~n Rows = ~p~n", [Ncols, Rows]),
        
    C = getClient(ClientAtom),

    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),

    ColRunFun = 
	fun(Ncol) ->
		[tsLatencyQueryTest({C, Select, Group, Limit, Filter, Put, IntervalMs}, Nrow, Ncol, Niter, Nbyte) || {Niter, Nrow, Put} <- Rows]
	end,

    [ColRunFun(Ncol) || Ncol <- Ncols].

tsLatencyQueryTest({C, Select, Group, Limit, Filter, PutData, IntervalMs}, Nrow, Ncol, Niter, Nbyte) ->

    FieldData = tsLatencyTestData(Ncol, Nbyte),
    Bucket = "Gen" ++ integer_to_list(Ncol),

    %%------------------------------------------------------------
    %% Put generated data, but only if requested (if we put data for
    %% the largest nrow first, we don't need to subsequently)
    %%------------------------------------------------------------

    case PutData of 
	true ->
	    io:format("Putting data for Ncol = ~p Nrow = ~p ~n", [Ncol, Nrow]),
	    putQueryTestData({C, Bucket, FieldData, IntervalMs}, Nrow, 0);
	_ ->
	    ok
    end,
    
    Name  = list_to_atom("query_" ++ integer_to_list(Ncol) ++ "_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(Nrow) ++ "_" ++ integer_to_list(Niter)),

    %%------------------------------------------------------------
    %% select * if Select == all, or just what was specified
    %%------------------------------------------------------------

    FieldCond = 
	case Select of
	    all ->
		"*";
	    _ ->
		atom_to_list(Select)
	end,

    %%------------------------------------------------------------
    %% If Group is none, just range scan, else group the results
    %%------------------------------------------------------------

    GroupCond = 
	case Group of
	    none ->
		"";
	    _ ->
		" group by " ++ atom_to_list(Group)
	end,

    %%------------------------------------------------------------
    %% If Limit is none, just range scan, else apply a limit to
    %% exercise query buffers
    %%------------------------------------------------------------

    LimitCond = 
	case Limit of
	    nolimit ->
		"";
	    _ ->
		" limit " ++ integer_to_list(Nrow)
	end,

    %%------------------------------------------------------------
    %% If Filter is none, just range scan, else apply a secondary
    %% filter that will match all keys we are iterating over
    %%------------------------------------------------------------

    FilterCond = 
	case Filter of
	    none ->
		"";
	    _ ->
		" and myint <= " ++ integer_to_list(Nrow)
	end,

    Query = "select " ++ FieldCond ++ " from Gen" ++ integer_to_list(Ncol) ++ 
	" where myfamily='family1' and myseries='seriesX' and time > 0 and time <= " ++ 
	integer_to_list(Nrow*IntervalMs) ++ FilterCond ++ GroupCond ++ LimitCond,

    %%------------------------------------------------------------
    %% Now start the profiler for Niter iterations of this combination
    %% rows, columns and bytes per column
    %%------------------------------------------------------------

    io:format("Querying data for Ncol = ~p Nbyte = ~p Nrow = ~p with Query = ~p~n", [Ncol, Nbyte, Nrow, Query]),
    io:format("~p About to start profile for Name ~p~n", [self(), Name]),

    profiler:profile({start, Name}),
    tsLatencyQueryTest({C, Query, Name}, Niter, 0).

tsLatencyQueryTest({_C, _Query, Name}, _Niter, _Niter) ->
    profiler:profile({stop, Name}),
    ok;
tsLatencyQueryTest(Args, Niter, AccIter) ->
    {C, Query, _Name} = Args,
    {ok, _} = riakc_ts:query(C, Query),
    tsLatencyQueryTest(Args, Niter, AccIter+1).

putQueryTestData(Args, Nrow, AccRow) ->
    putQueryTestData(Args, Nrow, AccRow, 0).

putQueryTestData(_Args, _Nrow, _Nrow, _Ind) ->
    ok;
putQueryTestData(Args, Nrow, AccRow, _GenInd) ->
    {C, Bucket, FieldData, IntervalMs} = Args,

    %%------------------------------------------------------------
    %% We will generate data with the same integer index for 10% of
    %% the total data length
    %%------------------------------------------------------------

%%    RowsPerInd = 
%%	case Nrow > 10 of
%%	    true ->
%%		Nrow div 10;
%%	    _ ->
%%		1
%%	end,

%%    NewInd = 
%%	case AccRow rem RowsPerInd of 
%%	    0 ->
%%		random:uniform(100);
%%	    _ ->
%%		GenInd
%%	end,
    
    NewInd = AccRow + 1,

    %% Write data in intervals of IntervalMs

    Data = [list_to_tuple([<<"family1">>, <<"seriesX">>, (AccRow+1)*IntervalMs] ++ FieldData ++ [NewInd])],

    ok = riakc_ts:put(C, Bucket, Data),

%    Resp = ok,

%    case AccRow of
%	0 ->
%	    io:format("Put bucket = ~p ~n Data = ~p~n Resp = ~p~n", [Bucket, Data, Resp]);
%	_ ->
%	    ok
%   end,

    putQueryTestData(Args, Nrow, AccRow+1, NewInd).

%%=======================================================================
%% KV 2i Query Latency Tests
%%=======================================================================

runKv2iQueryLatencyTests([ArgStr]) ->
    ArgDict = getArgDict(ArgStr),

    Client = getArgKey(ArgDict, client, local),
    Nbyte  = getArgKey(ArgDict, nbyte,  100),
    Nkeys  = getArgKey(ArgDict, nkeys,  [30000, 60000, 90000, 120000, 180000]),

    Rows   = [{10,     30000, true}, 
	      {10,     10000, false}, 
	      {10,      5000, false}, 
	      {10,      1000, false}, 
	      {10,       100, false}, 
	      {10,        10, false}],
    
    C = getClient(Client),
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),

    KeyRunFn = 
	fun(Nkey) ->
		[kv2iQueryLatencyTest({C, Nbyte, Nrow, Put, Nkey}, Niter) || {Niter, Nrow, Put} <- Rows]
	end,

    [KeyRunFn(Nkey) || Nkey <- Nkeys].

%%------------------------------------------------------------
%% Optionally put data, then run queries
%%------------------------------------------------------------

kv2iLatencyTestData(Nbyte) ->
    [crypto:rand_bytes(Nbyte)].

kv2iQueryLatencyTest({C, Nbyte, Nrow, PutData, Nkey}, Niter) ->

    %%------------------------------------------------------------
    %% Put generated data, but only if requested (if we put data for
    %% the largest nrow first, we don't need to subsequently)
    %%------------------------------------------------------------

    case PutData of 
	true ->
	    io:format("Putting Nkeys = ~p of data for Nbyte = ~p ~n", [Nkey, Nbyte]),
	    kv2iContent(Nbyte, Nkey, 10);
	_ ->
	    ok
    end,

    Name  = list_to_atom("query_" ++ integer_to_list(Nkey) ++ "_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(Nrow) ++ "_" ++ integer_to_list(Niter)),

    profiler:profile({start, Name}),
    kv2iQueryLatencyTest({C, Nrow, Name}, Niter, 0).

kv2iQueryLatencyTest({_C, _Nrow, Name}, _Niter, _Niter) ->
    profiler:profile({stop, Name}),
    ok;
kv2iQueryLatencyTest(Args, Niter, AccIter) ->
    {C, Nrow, _Name} = Args,
    {ok, {index_results_v1, _ResList, undefined, undefined}} = 
	riakc_pb_socket:get_index(C, {<<"TestBucketType">>, <<"2ibucket">>}, {integer_index, "myind"}, 1, Nrow),
    kv2iQueryLatencyTest(Args, Niter, AccIter+1).

%%------------------------------------------------------------
%% Write 2i query test data
%%------------------------------------------------------------

put2iQueryTestData(_Args, _Nrow, _Nrow) ->
    ok;
put2iQueryTestData(Args, Nrow, AccRow) ->

    {C, FieldData} = Args,

    %% Write data in intervals of IntervalMs

    Key = list_to_binary("key" ++ integer_to_list(AccRow)),
    Obj = riakc_obj:new({<<"TestBucketType">>, <<"2ibucket">>}, Key, FieldData),
    MD1 = riakc_obj:get_update_metadata(Obj),
    MD2 = riakc_obj:set_secondary_index(
	    MD1,
	    [{{integer_index, "myind"}, [AccRow+1]}]),
    Obj2 = riakc_obj:update_metadata(Obj, MD2),

    riakc_pb_socket:put(C, Obj2),

    put2iQueryTestData(Args, Nrow, AccRow+1).

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

putSequentialIntellicoreData(sl, Nrow, {StartTime, StopTime, LapsFrac}) ->
    C = getClient(sl),
    Bucket = <<"TIM_motorsport_formula_e_2015">>,
    Delta = round((StopTime - StartTime) / Nrow),
    putSequentialIntellicoreData({C, Bucket, StartTime, Delta, LapsFrac}, Nrow, 0);

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

intellicoreTest(sl) ->
    putSequentialIntellicoreData(sl, 137000, {1467554400000, 1467563400000, 1404.0/137000});

intellicoreTest(N) ->
    putSequentialIntellicoreData(N, {1467554400000, 1467563400000, 1404.0/137000}).

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

%%=======================================================================
%% SE testing
%%=======================================================================

seTest(Nsensor, Nsec, NthreadPerSensor) ->
    seBucketTest(<<"sensor_values_by_outstation">>, Nsensor, Nsec, NthreadPerSensor).

sepTest(Nsensor, Nsec, NthreadPerSensor) ->
    seBucketTest(<<"sensor_values_by_outstation_p">>, Nsensor, Nsec, NthreadPerSensor).

%% Fire off Nthread threads, writing sequential data for a single sensor

seBucketTest(Bucket, Nsensor, Nsec, NthreadPerSensor) ->

    %% Pick a start date

    MsStart = 1467554400000,
    MsEnd   = MsStart + Nsec*1000, %% Nsec worth of data
    MsDelta = 1000, %% Quantized to 1-second

    %% The total number of rows we will write

    NrowTotal = round((MsEnd - MsStart) / MsDelta),

    %% The number of rows each thread will write

    NrowPerThread = NrowTotal div NthreadPerSensor,

    %% For each sensor, we iterate over time indices

    SensorFn = 
	fun(SensorIndex) ->
		[spawn(riak_prof_tests, sePutThreadFn, [{Bucket, MsStart, MsDelta, NrowPerThread}, SensorIndex, TimeIndex, self()]) || TimeIndex <- lists:seq(0, NthreadPerSensor-1)]
	end,

    %% Iterate over sensors

    [SensorFn(SensorIndex) || SensorIndex <- lists:seq(1, Nsensor)],
    
    waitForResponses(Nsensor * NthreadPerSensor).

sePutThreadFn(Args, SensorIndex, TimeIndex, Pid) ->
    io:format("Sensor ~p Time ~p starting~n", [SensorIndex, TimeIndex]),
    C = getClient(),
    {Bucket, MsStart, MsDelta, Nrow} = Args,
    sePutFn({C, Bucket, MsStart, MsDelta, SensorIndex, TimeIndex}, Nrow, 0, Pid).

sePutFn(_Args, _Nrow, _Nrow, Pid) ->
    Pid ! {finished, self()};
sePutFn(Args, Nrow, Acc, Pid) ->
    {C, Bucket, MsStart, MsDelta, SensorIndex, TimeIndex} = Args,
    Time = MsStart + (TimeIndex*Nrow + Acc) * MsDelta,
    Data = [list_to_tuple([<<"outstation">>, Time, list_to_binary("id"++integer_to_list(SensorIndex)), list_to_binary(get_random_string(10))])],
    riakc_ts:put(C, Bucket, Data),
    case Acc rem 1000 of 
	0 ->
	    io:format("Sensor ~p Rel time ~p row ~p Data = ~p~n", [SensorIndex, (TimeIndex*Nrow + Acc), Acc, Data]);
	_ ->
	    ok
    end,
    sePutFn(Args, Nrow, Acc+1, Pid).

seQuery(Tag, Hours) ->
    profiler:profile({start, Tag}),
    query("select * from sensor_values_by_outstation where id='id1' and time >= 1467554400000 and time <= " ++ integer_to_list(1467554400000+Hours*3600*1000) ++ " and outstation='outstation'", local),
    profiler:profile({stop, Tag}),
    profiler:profile({debug}).

sepQuery(Tag, Hours) ->
    profiler:profile({start, Tag}),
    query("select * from sensor_values_by_outstation_p where id='id1' and time >= 1467554400000 and time < " ++ integer_to_list(1467554400000+Hours*3600*1000) ++ " and outstation='outstation'", local),
    profiler:profile({stop, Tag}),
    profiler:profile({debug}).
	 
%%----------------------------------------------------------------------- 
%% KV PUT/GET Latency tests
%%----------------------------------------------------------------------- 

runKvPutLatencyPercentileTest(N) ->
    Bytes = [      1,
	          10, 
	         100, 
	        1000, 
	       10000, 
	      100000, 
	     1000000, 
	    10000000],
    C = getClient(),
    [kvPutLatencyPercentileTest(C, N, Nbyte) || Nbyte <- Bytes].

runKvLatencyTests([ArgStr]) ->
    ArgDict = getArgDict(ArgStr),
    Client  = getArgKey(ArgDict, "client", local),
    Bucket  = list_to_binary(getArgKey(ArgDict, "bucket", "TestBucketType")),
    io:format("Client = ~p Bucket - ~p~n Dict = ~p~n", [Client, Bucket, ArgDict]),

    Bytes = [{100,       1}, 
	     {100,      10}, 
	     {100,     100}, 
	     {100,    1000}, 
	     {100,   10000}, 
	     {100,  100000}, 
	     {100, 1000000}, 
	     {10, 10000000}],
    [runKvLatencyTest(N, Nbyte, Client, Bucket) || {N, Nbyte} <- Bytes].

runKvLatencyTest(N, Nbyte, Client, Bucket) ->
    kvPutTest(N, Nbyte, Client, Bucket),
    kvGetTest(N, Nbyte, Client, Bucket),
    kvDelTest(N, Nbyte, Client, Bucket).
    
runKvLatencyTest(Args) ->
    [Nstr, NbyteStr] = Args,
    N = list_to_integer(Nstr),
    Nbyte = list_to_integer(NbyteStr),
    kvPutTest(N, Nbyte, local, <<"TestBucketType">>),
    kvGetTest(N, Nbyte, local, <<"TestBucketType">>).
     
kvTestData(Nbyte) ->
    crypto:rand_bytes(Nbyte).

kvPutTest(N,Nbyte,Client,Bucket) ->
    C = getClient(Client),
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),
    Name = list_to_atom("put_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(N)),
    Data = kvTestData(Nbyte),
    profiler:profile({start, Name}),
    kvPutTest(C,Bucket,Data,Name,N,0).
kvPutTest(_C,_Bucket,_Data,Name,_N,_N) ->
    profiler:profile({stop, Name}),
    profiler:profile({debug}),
    ok;
kvPutTest(C,Bucket,Data,Name, N,Acc) ->
    Key = list_to_binary("key" ++ integer_to_list(Acc)),
    Obj = riakc_obj:new({Bucket, <<"GeoCheckin">>}, Key, Data),
    riakc_pb_socket:put(C, Obj),
    kvPutTest(C,Bucket,Data,Name,N,Acc+1).

<<<<<<< HEAD
kvPutLatencyPercentileTest(C, N, Nbyte) ->
    Name = list_to_atom("put_nbyte" ++ integer_to_list(Nbyte) ++ "_nrow" ++ integer_to_list(N)),
    FileName = "/tmp/kvlatency_perc/" ++ atom_to_list(Name),
    case file:open(FileName, [append]) of
        {ok, IoDevice} ->
	    IoDevice,
	    io:format("Generating data ~n", []),
	    Data = kvTestData(Nbyte),
	    kvPutLatencyPercentileTestLoop({C, Data, IoDevice}, N, 0);
	{error, Reason} ->
            io:format("~s open error  reason:~s~n", [FileName, Reason])
    end.

kvPutLatencyPercentileTestLoop({_C,_Data, IoDevice},_N,_N) ->
    file:close(IoDevice),
    ok;
kvPutLatencyPercentileTestLoop({C, Data, IoDevice}, N, Acc) ->
    Key = list_to_binary("key" ++ integer_to_list(Acc)),
    Obj = riakc_obj:new({<<"TestBucketType">>, <<"GeoCheckin">>}, Key, Data),

    StartTime = eleveldb:current_usec(),
    riakc_pb_socket:put(C, Obj),
    StopTime = eleveldb:current_usec(),

    Bytes = io_lib:format("~p~n", [StopTime - StartTime]),
    file:write(IoDevice, Bytes),

    kvPutLatencyPercentileTestLoop({C, Data, IoDevice}, N, Acc+1).

kvGetTest(N,Nbyte) ->
    C = getClient(),
=======
kvGetTest(N,Nbyte,Client,Bucket) ->
    C = getClient(Client),
>>>>>>> 877b761e14ed554716ebc02b2778327fb834b242
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),
    Name = list_to_atom("get_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(N)),
    profiler:profile({start, Name}),
    kvGetTest(C,Bucket,Name,N,0).
kvGetTest(_C,_Bucket,Name,_N,_N) ->
    profiler:profile({stop, Name}),
    profiler:profile({debug}),
    ok;
kvGetTest(C,Bucket,Name,N,Acc) ->
    Key = list_to_binary("key" ++ integer_to_list(Acc)),
    riakc_pb_socket:get(C, {Bucket, <<"GeoCheckin">>}, Key),
    kvGetTest(C,Bucket,Name,N,Acc+1).

kvDelTest(N,Nbyte,Client,Bucket) ->
    C = getClient(Client),
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),
    Name = list_to_atom("del_" ++ integer_to_list(Nbyte) ++ "_" ++ integer_to_list(N)),
    profiler:profile({start, Name}),
    kvDelTest(C,Bucket,Name,N,0).
kvDelTest(_C,_Bucket,Name,_N,_N) ->
    profiler:profile({stop, Name}),
    profiler:profile({debug}),
    ok;
kvDelTest(C,Bucket,Name,N,Acc) ->
    Key = list_to_binary("key" ++ integer_to_list(Acc)),
    riakc_pb_socket:delete(C, {Bucket, <<"GeoCheckin">>}, Key),
    kvDelTest(C,Bucket,Name,N,Acc+1).

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
	C = getClient(Atom),
	{ok, {_Cols, Rows}} = riakc_ts:query(C, Q),
        io:format("Query returned ~p rows~n~p~n", [length(Rows), Rows]),
        Rows.

query(Q, Ip, Port) ->
	C = getClient(Ip, Port),
	{ok, {_Cols, Rows}} = riakc_ts:query(C, Q),
        io:format("Query returned ~p rows~n~p~n", [length(Rows), Rows]),
        Rows.

geoQueryTest() ->
    query("select * from GeoCheckin where myfamily='family1' and myseries='seriesX' and time >= 0 and time <= 10000", "127.0.0.1", 10017).

mqttQueryTest(StartTime, EndTime) ->
    query("select * from GeoCheckin where myfamily='familyMqtt' and myseries='seriesMqtt' and time >= " ++ integer_to_list(StartTime) ++ " and time <= " ++ integer_to_list(EndTime), "127.0.0.1", 10017).

timedQuery(Q, Atom, ProfAtom) ->
    C = getClient(Atom),
    profiler:profile({start, ProfAtom}),
    {ok, {_Cols, Rows}} = riakc_ts:query(C, Q),
    profiler:profile({stop, ProfAtom}),
    profiler:profile({debug}),
    io:format("Query returned ~p rows~n", [length(Rows)]),
    Rows.

iq(all, Atom, ProfAtom) ->
    Q = "select * from TIM_motorsport_formula_e_2015 where sport_event_uuid = '596044d8-86f5-462f-8d94-65b25e7d3fe9' and time >= 1467554400000 and time < 1467563400000",
    timedQuery(Q, Atom, ProfAtom);
iq(laps, Atom, ProfAtom) ->
    Q = "select * from TIM_motorsport_formula_e_2015 where sport_event_uuid = '596044d8-86f5-462f-8d94-65b25e7d3fe9' and time >= 1467554400000 and time < 1467563400000 and laps > 0",
    timedQuery(Q, Atom, ProfAtom).

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

    {ok, Key, _Val} = eleveldb:iterator_move(Iter, first),
%%    io:format("Key = ~p Val = ~p~n", [Key, Val]),
    printNextKey(DbRef, Iter, [Key]).

printNextKey(DbRef, Iter, Keys) ->
    try
	{ok, Key, Val} = eleveldb:iterator_move(Iter, next),
	DecodedKey = riak_kv_eleveldb_backend:orig_from_object_key(Key),
	Obj = riak_object:from_binary(<<"GeoCheckin">>, Key, Val),
	DecodedValue = riak_object:get_values(Obj),
	io:format("Raw Key = ~p ~n", [Key]),
	io:format("Raw Val = ~p Obj = ~p~n", [Val, Obj]),
	io:format("Key = ~p Value = ~p~n", [DecodedKey, DecodedValue]),
%%	io:format("Eleveldb tests = ~p~n", [eleveldb:get(DbRef, <<16,0,0,0,3,12,183,128,8,18,161,0,8,18,165,128,8>>, [])]),
	printNextKey(DbRef, Iter, [Keys, DecodedKey])
    catch
	error:{badmatch,{error,invalid_iterator}} ->
	    eleveldb:iterator_close(Iter),
	    eleveldb:close(DbRef),
	    [Keys, done];
	_ ->
	    [Keys, error]
    end.
       

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
	_:_ ->
	    0
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

countKeysSF(File) ->
    LockFile = File ++ "/LOCK",
    os:cmd("rm " ++ LockFile),
    Opts = [{total_memory, 14307360768}, {block_cache_threshold, 33554432}, {block_restart_interval, 16}, {block_size_steps, 16}, {cache_object_warming, true}, {compression, lz4}, {create_if_missing, true}, {delete_threshold, 1000}, {eleveldb_threads, 71}, {expiry_enabled, false}, {expiry_minutes, 0}, {fadvise_willneed, false}, {limited_developer_mem, true}, {sst_block_size, 4096}, {tiered_slow_level, 0}, {total_leveldb_mem_percent, 70}, {use_bloomfilter, true}, {whole_file_expiry, true}, {write_buffer_size, 39817495}],

    {ok, DbRef} = eleveldb:open(File, Opts),

    FoldOpts=[{fold_method, streaming},
	      {encoding, msgpack}],

    FF = fun({K,_V}, Acc) -> 
%%		 io:format("Found Key ~p~n Decoded = ~p~n", [K, riak_kv_eleveldb_backend:orig_from_object_key(K)]),
		 [K | Acc]
	 end,

    try 
	Acc = eleveldb:fold(DbRef, FF, [], FoldOpts),
	ok = eleveldb:close(DbRef),

	N = length(Acc),
	case N > 0 of
	    true ->
		N-1;
	    _ ->
		0
	end
    catch
	error:_Error ->
	    io:format(user, "Caught an error: closing db~n", []),
	    ok = eleveldb:close(DbRef),
	    0
    end.

countKeySF(DbRef, Iter, Acc) ->
    case eleveldb:iterator_move(Iter, next) of
	{ok, _Key, _Val} ->
	    countKeySF(DbRef, Iter, Acc+1);
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

printLeveldbKeys() ->
    printLeveldbKeys(dbFiles()).

countLeveldbKeysSF([File]) when is_list(File) ->
    io:format("~p~n", [countKeysSF(File)]);
countLeveldbKeysSF(List) ->
    [io:format("~p~n", [countKeysSF(File)]) || File <- List].

countLeveldbKeys([File]) when is_list(File) ->
    io:format("~p~n", [countKeys(File)]);
countLeveldbKeys(List) ->
    [io:format("~p~n", [countKeys(File)]) || File <- List].

testFn([File]) when is_list(File) ->
    io:format("~p~n", [File]).

%%=======================================================================
%% Functions for spawning threads
%%=======================================================================

-record(generator_args, {
	  fixedVals :: list(),
	  startTime :: integer(),
	  tsIncrement :: integer()
	 }).

-record(thread_content, {
          connectionPool :: list(),
	  nIter :: integer(),
	  generatorFn :: fun(),
	  generatorArgs :: #generator_args{}
         }).

-type thread_content() :: #thread_content{}.

%%------------------------------------------------------------
%% Top-level spawn function:
%%
%%    Fn      - is the function to spawn
%%    Args    - args passed to each thread running Fn (in addition to Acc -- thread #)
%%    NThread - number of threads to spawn
%% 
%%    NOp     - Used to construct a tag for profiling
%%    OpTag   - Used to construct a tag for profiling
%%------------------------------------------------------------

spawnFn(Fn, Args=#thread_content{}, NThread, OpTag) ->
    spawnFn(Fn, Args, NThread, getNiter(Args), OpTag).

spawnFn(Fn, Args, NThread, NOp, OpTag) ->
    profiler:profile({prefix, "/tmp/client_profiler_results"}),
    profiler:profile({noop, false}),
    Tag = list_to_atom(OpTag ++ "_" ++ integer_to_list(NThread) ++ "_" ++ integer_to_list(NOp)),

    profiler:profile({start, Tag}),

    doSpawn(Fn, Args, NThread),
    waitForResponses(NThread),

    profiler:profile({stop, Tag}),
    profiler:profile({debug}).

%%------------------------------------------------------------
%% Spawn NThread threads running Fn
%%------------------------------------------------------------

doSpawn(Fn, Args, NThread) ->
    doSpawn(Fn, Args, NThread, 0).
doSpawn(_Fn, _Args, _NThread, _NThread) ->
    ok;
doSpawn(Fn, Args, NThread, Acc) ->
    spawn(riak_prof_tests, Fn, [Args, Acc, self()]),
    doSpawn(Fn, Args, NThread, Acc+1).

%%------------------------------------------------------------
%% Wait for responses from N threads
%%------------------------------------------------------------

waitForResponses(N) ->
    waitForResponse(N,0).
waitForResponse(_N,_N) ->
    ok;
waitForResponse(N,Acc) ->
    receive
	{finished, _Pid} ->
	    waitForResponse(N,Acc+1)
    end.

%%------------------------------------------------------------
%% Put Nkeys of data into bucket Bucket with GeneratorFn
%%------------------------------------------------------------

newGeneratorArgs(FixedVals) ->
    newGeneratorArgs(FixedVals, 0, 0).

newGeneratorArgs(FixedVals, StartTime, TsIncrement) ->
    #generator_args{fixedVals=FixedVals, startTime=StartTime, tsIncrement=TsIncrement}.

newThreadContent(N, Niter, GeneratorFn, GeneratorArgs) when is_integer(N) ->
    newThreadContent(connectionPool(N, 10017), Niter, GeneratorFn, GeneratorArgs);
newThreadContent(ConnectionPool, Niter, GeneratorFn, GeneratorArgs) ->
    #thread_content{connectionPool=ConnectionPool, nIter=Niter, generatorFn=GeneratorFn, generatorArgs=GeneratorArgs}.

connectionPool(N, StartPort) when is_integer(N) ->
    [{"127.0.0.1", StartPort+Ind*10} || Ind <- lists:seq(0,N-1)].

getGeneratorArgs(Content) ->
    Content#thread_content.generatorArgs.

getConnectionPool(Content) ->
    Content#thread_content.connectionPool.

getGeneratorFn(Content) ->
    Content#thread_content.generatorFn.

getNiter(Content) ->
    Content#thread_content.nIter.

getUniformRandClient(Content, ThreadNo) ->
    Pool = getConnectionPool(Content),
    {Ip, Port} = lists:nth((ThreadNo rem length(Pool))+1, Pool),
    io:format("Getting client connection on ~p ~p ~p ~p~n", [Ip, Port, Pool, random:uniform(1024)]),
    getClient(Ip, Port).

generatorFn(geo) ->
    fun(GenArgs, ThreadNo, Niter, Iter) ->    
	    geoCheckinGeneratorFn(GenArgs, ThreadNo, Niter, Iter)
    end;
generatorFn(kv2i) ->
    fun(GenArgs, ThreadNo, Niter, Iter) ->    
	    kv2iGeneratorFn(GenArgs, ThreadNo, Niter, Iter)
    end.

%%------------------------------------------------------------
%% Generic function to get a client connection from the connection
%% pool, and put Niter records of data via that client
%%------------------------------------------------------------

-spec tsPutThreadFn(Content::thread_content(), ThreadNo::integer(), Pid::pid()) -> atom().

tsPutThreadFn(Content, ThreadNo, Pid) ->
    C       = getUniformRandClient(Content, ThreadNo),
    GenFn   = getGeneratorFn(Content),
    GenArgs = getGeneratorArgs(Content),
    Niter   = getNiter(Content),

    io:format("Niter = ~p ~n", [Niter]),

    tsPutThreadFn({C, GenFn, GenArgs, ThreadNo}, Niter, 0, Pid).
tsPutThreadFn(_Args, _Niter, _Niter, Pid) ->
    Pid ! {finished, self()};
tsPutThreadFn(Args, Niter, Iter, Pid) ->
    {C, GenFn, GenArgs, ThreadNo} = Args,
    {Bucket, Data} = GenFn(GenArgs, ThreadNo, Niter, Iter),
    riakc_ts:put(C, Bucket, Data),
    tsPutThreadFn(Args, Niter, Iter+1, Pid).

%% Sample GeoCheckin generator function

geoContent(Ntotal, Nthread) ->
    GenArgs = riak_prof_tests:newGeneratorArgs([<<"series1">>, <<"family1">>], 1, 100),
    GenFn   = riak_prof_tests:generatorFn(geo),
    Niter   = Ntotal div Nthread,
    Tag     = integer_to_list(Niter) ++ "_" ++ integer_to_list(Nthread),
    Content = riak_prof_tests:newThreadContent(3, Niter, GenFn, GenArgs),

    spawnFn(tsPutThreadFn, Content, Nthread, Tag).

geoCheckinGeneratorFn(GenArgs, ThreadNo, _Niter, Iter) ->    
%%    FixedVals=GenArgs#generator_args.fixedVals,
    StartTime   = GenArgs#generator_args.startTime,
    TsIncrement = GenArgs#generator_args.tsIncrement,
    Data = [{list_to_binary("family" ++ integer_to_list(ThreadNo)), 
	     list_to_binary("series" ++ integer_to_list(ThreadNo)), 
	     StartTime + Iter * TsIncrement, 
	     1024, 
	     <<"bin">>, 
	     1.024, 
	     false}],
    {<<"GeoCheckin">>, Data}.

%%------------------------------------------------------------
%% Put Niter keys of Nbyte-size data to the KV bucket
%%------------------------------------------------------------

kvPutThreadFn(Args, Acc, Pid) ->
    C = getClient(),
    {Niter, Nbyte} = Args,
    Data = kvTestData(Nbyte),
    kvPutThreadFn({C, Data, Acc}, Niter, 0, Pid).
kvPutThreadFn(_Args, _Niter, _Niter, Pid) ->
    Pid ! {finished, self()};
kvPutThreadFn(Args, Niter, Acc, Pid) ->
    {C, Data, ThreadId} = Args,
    Key = list_to_binary("key" ++ integer_to_list(ThreadId) ++ "_" ++ integer_to_list(Acc)),
    Obj = riakc_obj:new({<<"TestBucketType">>, <<"GeoCheckin">>}, Key, Data),
    riakc_pb_socket:put(C, Obj),
    kvPutThreadFn(Args, Niter, Acc+1, Pid).

%% Sample GeoCheckin generator function

kv2iContent(Nbyte, Ntotal, Nthread) ->
    GenArgs = riak_prof_tests:newGeneratorArgs(kvTestData(Nbyte)),
    io:format("GenArgs = ~p~n", [GenArgs]),
    GenFn   = riak_prof_tests:generatorFn(kv2i),
    Niter   = Ntotal div Nthread,
    Tag     = integer_to_list(Niter) ++ "_" ++ integer_to_list(Nthread),
    Content = riak_prof_tests:newThreadContent(1, Niter, GenFn, GenArgs),

    spawnFn(kv2iTestPutThreadFn, Content, Nthread, Tag).

kv2iGeneratorFn(GenArgs, ThreadNo, Niter, Iter) ->    
    Data=GenArgs#generator_args.fixedVals,
    Index = ThreadNo * Niter + Iter,
    Key = list_to_binary("key" ++ "_" ++ integer_to_list(Index)),
    Obj = riakc_obj:new({<<"TestBucketType">>, <<"2ibucket">>}, Key, Data),

    MD1 = riakc_obj:get_update_metadata(Obj),
    MD2 = riakc_obj:set_secondary_index(
	    MD1,
	    [{{integer_index, "myind"}, [Index]}]),
    riakc_obj:update_metadata(Obj, MD2).

kv2iTestPutThreadFn(Content, ThreadNo, Pid) ->
    C       = getUniformRandClient(Content, ThreadNo),
    GenFn   = getGeneratorFn(Content),
    GenArgs = getGeneratorArgs(Content),
    Niter   = getNiter(Content),

    kv2iTestPutThreadFn({C, GenFn, GenArgs, ThreadNo}, Niter, 0, Pid).
kv2iTestPutThreadFn(_Args, _Niter, _Niter, Pid) ->
    Pid ! {finished, self()};
kv2iTestPutThreadFn(Args, Niter, Iter, Pid) ->
    {C, GenFn, GenArgs, ThreadNo} = Args,
    Obj = GenFn(GenArgs, ThreadNo, Niter, Iter),
    riakc_pb_socket:put(C, Obj),
    kv2iTestPutThreadFn(Args, Niter, Iter+1, Pid).


kv2iPutTest() ->
    kv2iPutThreadFn({10, 10}, 0).

kv2iPutThreadFn(Args, Acc) ->
    kv2iPutThreadFn(Args, Acc, self()).

kv2iPutThreadFn(Args, Acc, Pid) ->
    C = getClient(),
    {Niter, Nbyte} = Args,
    Data = kvTestData(Nbyte),
    kv2iPutThreadFn({C, Data, Acc}, Niter, 0, Pid).
kv2iPutThreadFn(_Args, _Niter, _Niter, Pid) ->
    Pid ! {finished, self()};
kv2iPutThreadFn(Args, Niter, Acc, Pid) ->
    {C, Data, ThreadId} = Args,
    Key = list_to_binary("key" ++ integer_to_list(ThreadId) ++ "_" ++ integer_to_list(Acc)),
    Obj = riakc_obj:new({<<"TestBucketType">>, <<"2ibucket">>}, Key, Data),

    MD1 = riakc_obj:get_update_metadata(Obj),
    MD2 = riakc_obj:set_secondary_index(
	    MD1,
	    [{{integer_index, "myind"}, [Acc]}]),
    Obj2 = riakc_obj:update_metadata(Obj, MD2),

    io:format("Writing data to Key ~p with index ~p~n", [Key, Acc]),
    riakc_pb_socket:put(C, Obj2),

    kv2iPutThreadFn(Args, Niter, Acc+1, Pid).

kv2iquery(Ind) ->
    C = getClient(),
    riakc_pb_socket:get_index(C, {<<"TestBucketType">>, <<"2ibucket">>}, {integer_index, "myind"}, Ind).

kv2iquery(Ind1, Ind2) ->
    C = getClient(),
    riakc_pb_socket:get_index(C, {<<"TestBucketType">>, <<"2ibucket">>}, {integer_index, "myind"}, Ind1, Ind2).

kv2iQueryTest() ->
    kv2iPutTest(),
    kv2iquery(1,10).

quickQueryTest() ->
    C = getClient(),
    tsLatencyQueryTest({C, all, none, none}, 100000, 10, 3, 10).

put2i(N) ->
    C= getClient(),
    put2i(C, N, 0).

put2i(_C, _N, _N) ->
    ok;
put2i(C, N, Acc) ->
    put2iKey(C, Acc),
    put2i(C, N, Acc+1).

put2iKey(C, Acc) ->
    Obj = riakc_obj:new(<<"users">>,
			list_to_binary("key" ++ integer_to_list(Acc)),
			<<"...user data...">>,
			<<"text/plain">>),
    MD1 = riakc_obj:get_update_metadata(Obj),
    MD2 = riakc_obj:set_secondary_index(
	    MD1,
	    [{{integer_index, "myind"}, [Acc]}]),
    Obj2 = riakc_obj:update_metadata(Obj, MD2),
    riakc_pb_socket:put(C, Obj2).

get2i(Ind) ->
    Pid = getClient(),
    riakc_pb_socket:get_index(Pid,
                              <<"users">>, %% bucket
                              {integer_index, "myind"}, %% index name
                              Ind).
get2i(Ind1, Ind2) ->
    Pid = getClient(),
    riakc_pb_socket:get_index(Pid,
                              <<"users">>, %% bucket
                              {integer_index, "myind"}, %% index name
                              Ind1, Ind2).

putTestKey(Ind) ->
    Pid = getClient(),
    Obj = riakc_obj:new(<<"users">>,
			<<"john_smith">>,
			<<"...user data...">>,
			<<"text/plain">>),
    MD1 = riakc_obj:get_update_metadata(Obj),
    MD2 = riakc_obj:set_secondary_index(
	    MD1,
	    [{{integer_index, "myind"}, [Ind]}]),
    Obj2 = riakc_obj:update_metadata(Obj, MD2),
    riakc_pb_socket:put(Pid, Obj2).

putKvKey(Bucket, Key, Val) ->
    Pid = getClient(),
    Obj = riakc_obj:new(Bucket, Key, Val),
    riakc_pb_socket:put(Pid, Obj).

<<<<<<< HEAD

getModList() ->
    Ret = file:list_dir(app_helper:get_env(riak_core,platform_data_dir) ++ "/ddl_ebin"),
    
    case Ret of
        {error, enoent} ->
	    
            Ret;
	        {ok, FileList} ->
            Fn =
                fun(File) ->
                        EndLoc   = string:str(File, ".beam"),
                        list_to_atom(string:substr(File, 1, EndLoc-2))
                end,
            [Fn(File) || File <- FileList]
    end.

listDir() ->
    {ok, FileList} = file:list_dir("./data/ddl_ebin"),
    Fn =
        fun(File) ->
		StartLoc = string:str(File, "ddl_ebin/"),
		EndLoc   = string:str(File, ".beam"),
		string:substr(File, StartLoc+1, EndLoc-StartLoc-1)
	end,
    [Fn(File) || File <- FileList].

getCurrentSchema(OldSchemaSet) ->
    FileList = listDir(),
    Fn =
        fun(File) ->
		Mod = list_to_atom(File),
		{ddl_v2, Name, SchemaList, _Key1, _Key2, _Version} = Mod:get_ddl(),
		Schema = [Type || {_Vers, _Name, _Order, Type, _Null} <- SchemaList],
		{binary_to_list(Name), Schema}
	end,
    NewSchemaSet = sets:from_list([Fn(File) || File <- FileList]),
    NewSchemas = sets:subtract(NewSchemaSet, OldSchemaSet),
    {sets:to_list(NewSchemas), NewSchemaSet}.


findDDLPath() ->    
    L = code:get_path(),
    Fn = 
	fun(Entry) ->
		case string:str(Entry, "ddl_ebin") of
		    0 ->
			ok;
		    _ ->
			io:format("Entry = ~p~n", [Entry])
		end
	end,
    [Fn(Entry) || Entry <- L].
=======
geoQueryAws(Range) ->
    C = getClient("127.0.0.1", 8087),
    riakc_ts:query(C, "select * from GeoCheckin where myfamily='family1' and myseries='seriesX' and time > 0 and time < " ++ integer_to_list(Range)).
>>>>>>> 877b761e14ed554716ebc02b2778327fb834b242

