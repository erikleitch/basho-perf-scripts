%% -*- Mode: Erlang -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%% @doc A util module for riak_ts basic CREATE TABLE Actions

-module(ts_test_utils).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%=======================================================================
%% Bucket names
%%=======================================================================

%%------------------------------------------------------------
%% Generate a bucket name with appended integer
%%------------------------------------------------------------

get_bucket(Ind) when is_integer(Ind) ->
    "Gen" ++ integer_to_list(Ind);

%%------------------------------------------------------------
%% Else get standard bucket name
%%------------------------------------------------------------

get_bucket(geo) ->
    "GeoCheckin";

get_bucket(intellicore) ->
    "TIM_motorsport_formula_e_2015";

get_bucket(geoextra) ->
    "GeoCheckin";

%%------------------------------------------------------------
%% Else return the argument as the bucket name
%%------------------------------------------------------------

get_bucket(Bucket) ->
    Bucket.

%%=======================================================================
%% DDL generation
%%=======================================================================

genFields(Ind) ->
    genFields(Ind, 0, []).
genFields(_Ind, _Ind, Acc) ->
    Acc;
genFields(Ind, Counter, Acc) ->
    genFields(Ind, Counter+1, Acc ++ "myvar" ++ integer_to_list(Counter+1) ++ "  varchar  not null, ").

%%------------------------------------------------------------
%% Create a generated DDL with variable number of fields
%%------------------------------------------------------------

get_ddl(Ind) when is_integer(Ind) ->
    _SQL = "CREATE TABLE Gen" ++ integer_to_list(Ind) ++ " (" ++
	"myfamily    varchar   not null, " ++
	"myseries    varchar   not null, " ++
	"time        timestamp not null, " ++ 
	"myint       sint64    not null, " ++ genFields(Ind) ++ 
	"PRIMARY KEY ((myfamily, myseries, quantum(time, 15, 'm')), " ++
	"myfamily, myseries, time))";

%%------------------------------------------------------------
%% Standard DDL
%%------------------------------------------------------------

get_ddl(geo) ->
    _SQL = "CREATE TABLE GeoCheckin (" ++
	"myfamily    varchar     not null, " ++
	"myseries    varchar     not null, " ++
	"time        timestamp   not null, " ++
	"myint       sint64      not null, " ++
	"mybin       varchar     not null, " ++
	"myfloat     double      not null, " ++
	"mybool      boolean     not null, " ++
	"PRIMARY KEY ((myfamily, myseries, quantum(time, 15, 'm')), " ++
	"myfamily, myseries, time))";

%%------------------------------------------------------------
%% Non-standard DDL
%%------------------------------------------------------------

get_ddl(geoextra) ->
    _SQL = "CREATE TABLE GeoCheckin (" ++
	"myfamily    varchar     not null, " ++
	"myseries    varchar     not null, " ++
	"time        timestamp   not null, " ++
	"myint       sint64      not null, " ++
	"mybin       varchar     not null, " ++
	"myfloat     double      not null, " ++
	"mybool      boolean     not null, " ++
	"PRIMARY KEY ((myfamily, myseries, quantum(time, 15, 'm')), " ++
	"myfamily, myseries, time, myint))".

get_ddl(intellicore, Unit) ->
    get_ddl(intellicore, 1, Unit);

get_ddl(geo, Ms) ->
    _SQL = "CREATE TABLE GeoCheckin (" ++
	"myfamily    varchar     not null, " ++
	"myseries    varchar     not null, " ++
	"time        timestamp   not null, " ++
	"myint       sint64      not null, " ++
	"mybin       varchar     not null, " ++
	"myfloat     double      not null, " ++
	"mybool      boolean     not null, " ++
	"PRIMARY KEY ((myfamily, myseries, quantum(time, " ++ integer_to_list(Ms) ++ " , 's')), " ++
	"myfamily, myseries, time))".

get_ddl(intellicore, Mult, Unit) ->
    _SQL = "CREATE TABLE TIM_motorsport_formula_e_2015 (" ++
	"sport_event_uuid varchar  not null, " ++
	"time             timestamp not null, " ++
	"club_uuid        varchar  not null, " ++
	"person_uuid      varchar  not null, " ++
	"sport_uuid       varchar  not null, " ++
	"discipline_uuid  varchar  not null, " ++
	"driver_number    varchar  not null, " ++
	"person_full_name varchar  not null, " ++
	"club_full_name   varchar  not null, " ++
	"abandoned        boolean, " ++
	"best_gap_in_time double, " ++
	"best_gap_in_lap  sint64, " ++
	"best_lap         double, " ++
	"best_position    sint64, " ++
	"best_sector_1    double, " ++
	"best_sector_2    double, " ++
	"best_sector_3    double, " ++
	"best_speed       double, " ++
	"gap_in_time      double, " ++
	"gap_in_lap       sint64, " ++
	"lap_time         double, " ++
	"laps             sint64, " ++
	"position         sint64, " ++
	"qualification_position sint64, " ++
	"sector_1         double, " ++
	"sector_2         double, " ++
	"sector_3         double, " ++
	"info             varchar, " ++
	"PRIMARY KEY ((sport_event_uuid, quantum(time, " ++ integer_to_list(Mult) ++ ", '" ++ Unit ++ "')), " ++
	"sport_event_uuid, time, club_uuid, person_uuid))".

%%=======================================================================
%% Bucket creation
%%=======================================================================

create_kv_bucket(Node, NVal, Bucket, WriteOnce) when is_boolean(WriteOnce) ->
    Props = 
	case WriteOnce of
	    false ->
		io_lib:format("{\\\"props\\\": {" ++ 
				  "\\\"n_val\\\": " ++ integer_to_list(NVal) ++
				  ", \\\"allow_mult\\\": false" ++
				  "}}", []);
	    true ->
		io_lib:format("{\\\"props\\\": {\\\"n_val\\\": " ++ 
				  integer_to_list(NVal) ++
				  ", \\\"write_once\\\": true}}", [])
	end,
    Args = ["bucket-type", "create", Bucket, lists:flatten(Props)],
    rt:admin(Node, Args).

create_ts_bucket(Node, NVal, Bucket, DDL) ->
    Props = io_lib:format("{\\\"props\\\": {\\\"n_val\\\": " ++ 
			      integer_to_list(NVal) ++
			      ", \\\"table_def\\\": \\\"~s\\\"}}", [DDL]),
    Args = ["bucket-type", "create", Bucket, lists:flatten(Props)],
    rt:admin(Node, Args).

create_ts_bucket(Node, NVal, W, Bucket, DDL) ->
    Props = io_lib:format("{\\\"props\\\": {" ++ 
			      "\\\"n_val\\\": " ++ integer_to_list(NVal) ++
			      ", \\\"w\\\": " ++ integer_to_list(W) ++
			      ", \\\"dw\\\": " ++ integer_to_list(W) ++
			      ", \\\"table_def\\\": \\\"~s\\\"}}", [DDL]),
    Args = ["bucket-type", "create", Bucket, lists:flatten(Props)],
    rt:admin(Node, Args).

activate_bucket(Node, Bucket) ->
    rt:admin(Node, ["bucket-type", "activate", Bucket]).

%%=======================================================================
%% Cluster creation
%%=======================================================================

build_cluster(Size) when is_integer(Size) ->
    build_cluster(Size, []);
build_cluster(single) ->
    build_cluster(1);
build_cluster(single_bitcask) ->
    build_cluster(1, [], bitcask);
build_cluster(single_expiryon) ->
    build_cluster(1, [], expiryon);
build_cluster(single_expiryoff) ->
    build_cluster(1, [], expiryoff);
build_cluster(multiple) ->
    build_cluster(3);
build_cluster(multiple_bitcask) ->
    build_cluster(3, [], bitcask);
build_cluster(multiple_aae) ->
    build_cluster(3, [], eleveldb, on);
build_cluster(large) ->
    build_cluster(5);
build_cluster(intellicore) ->
    build_cluster(1, [], eleveldb, off, 128);
build_cluster(intellicore_multi) ->
    build_cluster(3, [], eleveldb, off, 128).

build_cluster(Size, Config) ->
    build_cluster(Size, Config, eleveldb).

build_cluster(Size, Config, expiryon) ->
    set_expiry(true),
    build_cluster(Size, Config, eleveldb);
build_cluster(Size, Config, expiryoff) ->
    set_expiry(false),
    build_cluster(Size, Config, eleveldb);

build_cluster(Size, Config, Backend) ->
    build_cluster(Size, Config, Backend, off).

build_cluster(Size, Config, Backend, Aae) ->
    build_cluster(Size, Config, Backend, Aae, 8).

build_cluster(Size, Config, Backend, Aae, Ringsize) ->
    rt:set_backend(Backend),
    set_aae_state(Aae),
    set_worker_pool_size(),
    set_max_object_size("50MB"),
    set_ring_size(Ringsize),
    [_Node1|_] = Nodes = rt:deploy_nodes(Size, Config),
    rt:join_cluster(Nodes),
    Nodes.

%%=======================================================================
%% Riak conf options
%%=======================================================================

set_riak_conf_option(OptName, OptVal) ->
    Opts = [{OptName, OptVal}],
    rtdev:set_conf(all, Opts).

set_riak_advanced_option(Module, OptName, OptVal) ->
    Opts = [{OptName, OptVal}],
    rtdev:update_app_config(all, [{Module, Opts}]).

set_riak_kv_option(OptName, OptVal) ->
    set_riak_advanced_option(riak_kv, OptName, OptVal).

set_riak_core_option(OptName, OptVal) ->
    set_riak_advanced_option(riak_core, OptName, OptVal).
    
set_aae_state(State) ->
    set_riak_kv_option(anti_entropy, {State, []}),
    case State of
	on ->
	    set_riak_conf_option(anti_entropy, active);
	off ->
	    set_riak_conf_option(anti_entropy, passive)
    end.

set_worker_pool_size() ->
    set_worker_pool_size(8).

set_worker_pool_size(N) ->
    set_riak_kv_option(worker_pool_size, N).

set_max_quanta(N) ->
    set_riak_kv_option(timeseries_query_max_quanta_span, N).

set_max_quanta() ->
    set_max_quanta(1000).

set_max_object_size(Mb) ->
    set_riak_kv_option(warn_object_size, Mb).

set_max_concurrent(N) ->
    Opts = [{timeseries_max_concurrent_queries, N}],
    rtdev:update_app_config(all, [{riak_kv, Opts}]).

set_ring_size(Ringsize) ->
    set_riak_core_option(ring_creation_size, Ringsize).

set_expiry(State) ->
    set_riak_advanced_option(eleveldb, expiry_enabled, State).

%%=======================================================================
%% Cluster setup
%%=======================================================================

%%------------------------------------------------------------
%% Normal TS cluster setup
%%------------------------------------------------------------

setup_ts_cluster(ClusterType, Nval, Api) ->
    [Node | _] = build_cluster(ClusterType),
    {ok, _} = create_ts_bucket(Node, Nval, get_bucket(Api), get_ddl(Api)),
    {ok, _} = activate_bucket(Node, get_bucket(Api)).

setup_ts_cluster(ClusterType, Nval, Api, Ms) ->
    [Node | _] = build_cluster(ClusterType),
    {ok, _} = create_ts_bucket(Node, Nval, get_bucket(Api), get_ddl(Api, Ms)),
    {ok, _} = activate_bucket(Node, get_bucket(Api)).

setup_ts_cluster(ClusterType, Nval, Api, Mult, Unit) ->
    [Node | _] = build_cluster(ClusterType),
    {ok, _} = create_ts_bucket(Node, Nval, get_bucket(Api), get_ddl(Api, Mult, Unit)),
    {ok, _} = activate_bucket(Node, get_bucket(Api)).

%%------------------------------------------------------------
%% Normal KV cluster setup
%%------------------------------------------------------------

setup_kv_cluster(ClusterType, Nval, Api, WriteOnce) ->
    [Node | _] = build_cluster(ClusterType),
    {ok, _} = create_kv_bucket(Node, Nval, get_bucket(Api), WriteOnce),
    {ok, _} = activate_bucket(Node, get_bucket(Api)).

%%------------------------------------------------------------
%% Setup a cluster with generated TS tables contining the numbers of
%% columns in ColList
%%------------------------------------------------------------

setup_ts_gen_cluster(ClusterType, Nval, ColList) ->
    [Node | _] = build_cluster(ClusterType),
    Fun = 
	fun(Ncol) ->
		DDL = get_ddl(Ncol),
		Bucket = "Gen" ++ integer_to_list(Ncol),
		io:format("Creating bucket ~p with DDL = ~p~n", [Bucket, DDL]),
		{ok, _} = create_ts_bucket(Node, Nval, Bucket, DDL),
		{ok, _} = activate_bucket(Node, Bucket)
	end,
    [Fun(Ncol) || Ncol <- ColList].

setup_ts_gen_cluster(ClusterType, Nval, W, ColList) ->
    [Node | _] = build_cluster(ClusterType),
    Fun = 
	fun(Ncol) ->
		DDL = get_ddl(Ncol),
		Bucket = "Gen" ++ integer_to_list(Ncol),
		io:format("Creating bucket ~p with DDL = ~p~n", [Bucket, DDL]),
		{ok, _} = create_ts_bucket(Node, Nval, W, Bucket, DDL),
		{ok, _} = activate_bucket(Node, Bucket)
	end,
    [Fun(Ncol) || Ncol <- ColList].

