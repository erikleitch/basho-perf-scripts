export RIAK_TEST_BASE=$(shell pwd)

all: compile

deps:
	@if [ ! -d erlang_scripts/profiler ] ;\
		then git clone "git@github.com:erikleitch/profiler.git" -b master erlang_scripts/profiler;\
	fi

compile: deps
	cd erlang_scripts; make

